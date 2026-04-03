(() => {
  const defaults = {
    p: 1,
    q: 1,
    P: 1,
    Q: 1,
    d: 1,
    D: 1,
    s: 12,
    acfLags: 48,
    pacfLags: 48
  };

  const clamp = (value, min, max) => Math.max(min, Math.min(max, value));

  function generateDefaultSeries(lags, type) {
    const values = [];
    for (let lag = 1; lag <= lags; lag += 1) {
      const seasonalBoost = lag % defaults.s === 0 ? 0.24 * Math.exp(-lag / 80) : 0;
      const base = type === "acf"
        ? 0.6 * Math.exp(-lag / 6)
        : lag <= 2
          ? 0.55 - (lag - 1) * 0.16
          : 0.22 * Math.exp(-lag / 8);
      const oscillation = Math.sin(lag * 0.45) * 0.05;
      values.push(clamp(base + seasonalBoost + oscillation, -0.98, 0.98));
    }
    return values;
  }

  class SpikeChart {
    constructor(options) {
      this.svg = options.svg;
      this.type = options.type;
      this.color = options.color;
      this.emphasisColor = "#d89b16";
      this.onEdit = options.onEdit;
      this.getHighlights = options.getHighlights;
      this.series = options.series;
      this.dragIndex = null;

      this.margin = { top: 20, right: 18, bottom: 35, left: 42 };
      this.viewBox = { width: 760, height: 320 };
      this.plot = {
        left: this.margin.left,
        right: this.viewBox.width - this.margin.right,
        top: this.margin.top,
        bottom: this.viewBox.height - this.margin.bottom
      };

      this.svg.addEventListener("pointerdown", (e) => this.onPointerDown(e));
      this.svg.addEventListener("pointermove", (e) => this.onPointerMove(e));
      this.svg.addEventListener("pointerup", () => this.onPointerUp());
      this.svg.addEventListener("pointerleave", () => this.onPointerUp());
      this.svg.addEventListener("lostpointercapture", () => this.onPointerUp());

      this.render();
    }

    setSeries(series) {
      this.series = series;
      this.render();
    }

    xAtIndex(i) {
      const width = this.plot.right - this.plot.left;
      return this.plot.left + (i + 1) * (width / (this.series.length + 1));
    }

    yAtValue(v) {
      const height = this.plot.bottom - this.plot.top;
      return this.plot.top + ((1 - v) / 2) * height;
    }

    valueFromY(y) {
      const height = this.plot.bottom - this.plot.top;
      const normalized = 1 - ((y - this.plot.top) / height) * 2;
      return clamp(normalized, -1, 1);
    }

    getEventPoint(evt) {
      const rect = this.svg.getBoundingClientRect();
      const scaleX = this.viewBox.width / rect.width;
      const scaleY = this.viewBox.height / rect.height;
      return {
        x: (evt.clientX - rect.left) * scaleX,
        y: (evt.clientY - rect.top) * scaleY
      };
    }

    nearestIndex(x) {
      let nearest = 0;
      let minDist = Number.POSITIVE_INFINITY;
      for (let i = 0; i < this.series.length; i += 1) {
        const dist = Math.abs(this.xAtIndex(i) - x);
        if (dist < minDist) {
          minDist = dist;
          nearest = i;
        }
      }
      return minDist <= 12 ? nearest : null;
    }

    onPointerDown(evt) {
      const point = this.getEventPoint(evt);
      const index = this.nearestIndex(point.x);
      if (index === null) {
        return;
      }
      this.dragIndex = index;
      this.svg.setPointerCapture(evt.pointerId);
      this.svg.classList.add("dragging");
      this.updatePointFromY(point.y);
    }

    onPointerMove(evt) {
      const point = this.getEventPoint(evt);
      if (this.dragIndex === null) {
        const hoverIndex = this.nearestIndex(point.x);
        this.svg.style.cursor = hoverIndex === null ? "default" : "ns-resize";
        return;
      }
      this.updatePointFromY(point.y);
    }

    onPointerUp() {
      this.dragIndex = null;
      this.svg.classList.remove("dragging");
    }

    updatePointFromY(y) {
      if (this.dragIndex === null) return;
      this.series[this.dragIndex] = this.valueFromY(y);
      this.onEdit();
      this.render();
    }

    render() {
      const ns = "http://www.w3.org/2000/svg";
      this.svg.innerHTML = "";

      const width = this.plot.right - this.plot.left;
      const height = this.plot.bottom - this.plot.top;
      const zeroY = this.yAtValue(0);

      const grid = document.createElementNS(ns, "g");
      grid.setAttribute("stroke", "#d8e0ef");
      grid.setAttribute("stroke-width", "1");

      [1, 0.5, 0, -0.5, -1].forEach((tick) => {
        const y = this.yAtValue(tick);
        const line = document.createElementNS(ns, "line");
        line.setAttribute("x1", this.plot.left);
        line.setAttribute("x2", this.plot.right);
        line.setAttribute("y1", y);
        line.setAttribute("y2", y);
        grid.appendChild(line);

        const label = document.createElementNS(ns, "text");
        label.setAttribute("x", this.plot.left - 8);
        label.setAttribute("y", y + 4);
        label.setAttribute("text-anchor", "end");
        label.setAttribute("font-size", "11");
        label.setAttribute("fill", "#5d6a7e");
        label.textContent = tick.toFixed(1);
        this.svg.appendChild(label);
      });
      this.svg.appendChild(grid);

      const axisX = document.createElementNS(ns, "line");
      axisX.setAttribute("x1", this.plot.left);
      axisX.setAttribute("x2", this.plot.right);
      axisX.setAttribute("y1", zeroY);
      axisX.setAttribute("y2", zeroY);
      axisX.setAttribute("stroke", "#9fb0cc");
      axisX.setAttribute("stroke-width", "2");
      this.svg.appendChild(axisX);

      const highlights = this.getHighlights();
      highlights.forEach((lag) => {
        if (lag < 1 || lag > this.series.length) return;
        const x = this.xAtIndex(lag - 1);
        const marker = document.createElementNS(ns, "line");
        marker.setAttribute("x1", x);
        marker.setAttribute("x2", x);
        marker.setAttribute("y1", this.plot.top);
        marker.setAttribute("y2", this.plot.bottom);
        marker.setAttribute("stroke", this.emphasisColor);
        marker.setAttribute("stroke-width", "1.2");
        marker.setAttribute("stroke-dasharray", "5 4");
        marker.setAttribute("opacity", "0.65");
        this.svg.appendChild(marker);
      });

      const spikes = document.createElementNS(ns, "g");
      this.series.forEach((value, i) => {
        const x = this.xAtIndex(i);
        const y = this.yAtValue(value);

        const spike = document.createElementNS(ns, "line");
        spike.setAttribute("x1", x);
        spike.setAttribute("x2", x);
        spike.setAttribute("y1", zeroY);
        spike.setAttribute("y2", y);
        spike.setAttribute("stroke", this.color);
        spike.setAttribute("stroke-width", this.dragIndex === i ? "5" : "3");
        spike.setAttribute("stroke-linecap", "round");
        spikes.appendChild(spike);

        const handle = document.createElementNS(ns, "circle");
        handle.setAttribute("cx", x);
        handle.setAttribute("cy", y);
        handle.setAttribute("r", this.dragIndex === i ? "5.5" : "4.2");
        handle.setAttribute("fill", this.dragIndex === i ? "#ffffff" : this.color);
        handle.setAttribute("stroke", this.color);
        handle.setAttribute("stroke-width", "2");
        spikes.appendChild(handle);
      });
      this.svg.appendChild(spikes);

      const labelStep = this.series.length > 72 ? 12 : this.series.length > 48 ? 8 : 6;
      for (let lag = labelStep; lag <= this.series.length; lag += labelStep) {
        const x = this.xAtIndex(lag - 1);
        const tick = document.createElementNS(ns, "line");
        tick.setAttribute("x1", x);
        tick.setAttribute("x2", x);
        tick.setAttribute("y1", this.plot.bottom);
        tick.setAttribute("y2", this.plot.bottom + 6);
        tick.setAttribute("stroke", "#8fa1be");
        tick.setAttribute("stroke-width", "1");
        this.svg.appendChild(tick);

        const text = document.createElementNS(ns, "text");
        text.setAttribute("x", x);
        text.setAttribute("y", this.plot.bottom + 19);
        text.setAttribute("text-anchor", "middle");
        text.setAttribute("font-size", "11");
        text.setAttribute("fill", "#5d6a7e");
        text.textContent = lag.toString();
        this.svg.appendChild(text);
      }

      const xAxisTitle = document.createElementNS(ns, "text");
      xAxisTitle.setAttribute("x", this.plot.left + width / 2);
      xAxisTitle.setAttribute("y", this.viewBox.height - 4);
      xAxisTitle.setAttribute("text-anchor", "middle");
      xAxisTitle.setAttribute("font-size", "12");
      xAxisTitle.setAttribute("fill", "#5d6a7e");
      xAxisTitle.textContent = "Lag";
      this.svg.appendChild(xAxisTitle);

      const yAxisTitle = document.createElementNS(ns, "text");
      yAxisTitle.setAttribute("x", 13);
      yAxisTitle.setAttribute("y", this.plot.top + height / 2);
      yAxisTitle.setAttribute("text-anchor", "middle");
      yAxisTitle.setAttribute("font-size", "12");
      yAxisTitle.setAttribute("fill", "#5d6a7e");
      yAxisTitle.setAttribute("transform", `rotate(-90 13 ${this.plot.top + height / 2})`);
      yAxisTitle.textContent = "Spike";
      this.svg.appendChild(yAxisTitle);
    }
  }

  const pRange = document.querySelector("#pRange");
  if (!pRange) return;

  const state = {
    p: defaults.p,
    q: defaults.q,
    P: defaults.P,
    Q: defaults.Q,
    d: defaults.d,
    D: defaults.D,
    s: defaults.s,
    acfLags: defaults.acfLags,
    pacfLags: defaults.pacfLags,
    acf: generateDefaultSeries(defaults.acfLags, "acf"),
    pacf: generateDefaultSeries(defaults.pacfLags, "pacf")
  };

  const controls = {
    p: pRange,
    q: document.querySelector("#qRange"),
    P: document.querySelector("#PRange"),
    Q: document.querySelector("#QRange"),
    acfLags: document.querySelector("#acfLags"),
    pacfLags: document.querySelector("#pacfLags")
  };

  const outputs = {
    p: document.querySelector("#pValue"),
    q: document.querySelector("#qValue"),
    P: document.querySelector("#PValue"),
    Q: document.querySelector("#QValue")
  };

  const modelDisplay = document.querySelector("#modelDisplay");
  const hintList = document.querySelector("#hintList");

  const seasonalLags = (count) => {
    const lags = [];
    for (let lag = state.s; lag <= count; lag += state.s) {
      lags.push(lag);
    }
    return lags;
  };

  const acfChart = new SpikeChart({
    svg: document.querySelector("#acfChart"),
    type: "acf",
    color: "#2f6dff",
    series: state.acf,
    onEdit: updateInfo,
    getHighlights: () => {
      const nonSeasonal = Array.from({ length: state.q }, (_, i) => i + 1);
      const seasonal = seasonalLags(state.acf.length).slice(0, state.Q);
      return [...nonSeasonal, ...seasonal];
    }
  });

  const pacfChart = new SpikeChart({
    svg: document.querySelector("#pacfChart"),
    type: "pacf",
    color: "#6a4df4",
    series: state.pacf,
    onEdit: updateInfo,
    getHighlights: () => {
      const nonSeasonal = Array.from({ length: state.p }, (_, i) => i + 1);
      const seasonal = seasonalLags(state.pacf.length).slice(0, state.P);
      return [...nonSeasonal, ...seasonal];
    }
  });

  function setControlText() {
    outputs.p.textContent = state.p.toString();
    outputs.q.textContent = state.q.toString();
    outputs.P.textContent = state.P.toString();
    outputs.Q.textContent = state.Q.toString();
  }

  function energy(series, fromLag, toLag) {
    let total = 0;
    const start = clamp(fromLag, 1, series.length);
    const end = clamp(toLag, 1, series.length);
    for (let lag = start; lag <= end; lag += 1) {
      total += Math.abs(series[lag - 1]);
    }
    return total / Math.max(1, end - start + 1);
  }

  function seasonalEnergy(series) {
    const lags = seasonalLags(series.length);
    if (!lags.length) return 0;
    const sum = lags.reduce((acc, lag) => acc + Math.abs(series[lag - 1]), 0);
    return sum / lags.length;
  }

  function hintItems() {
    const pacfEarly = energy(state.pacf, 1, Math.min(6, state.pacf.length));
    const acfEarly = energy(state.acf, 1, Math.min(6, state.acf.length));
    const pacfSeason = seasonalEnergy(state.pacf);
    const acfSeason = seasonalEnergy(state.acf);

    const hints = [];

    if (pacfEarly > 0.35) {
      hints.push("Strong early PACF spikes often suggest AR structure; consider whether p should be at least 1.");
    } else {
      hints.push("PACF is relatively mild at early lags; a small p can be a reasonable starting point.");
    }

    if (acfEarly > 0.35) {
      hints.push("Strong early ACF spikes often suggest MA structure; consider whether q should be at least 1.");
    } else {
      hints.push("ACF early lags are modest; MA terms may be limited unless diagnostics indicate otherwise.");
    }

    if (Math.max(acfSeason, pacfSeason) > 0.28) {
      hints.push("Seasonal spikes are noticeable around multiples of 12, which may justify seasonal terms P and/or Q.");
    } else {
      hints.push("Seasonal spikes look weak; seasonal orders might stay small if this reflects your real data.");
    }

    hints.push(`Current choice emphasizes AR(${state.p})/MA(${state.q}) with seasonal AR(${state.P})/MA(${state.Q}). Validate using residual checks.`);

    return hints;
  }

  function updateInfo() {
    modelDisplay.textContent = `SARIMA(${state.p},${state.d},${state.q})(${state.P},${state.D},${state.Q})[${state.s}]`;
    hintList.innerHTML = "";
    hintItems().forEach((text) => {
      const li = document.createElement("li");
      li.textContent = text;
      hintList.appendChild(li);
    });
    acfChart.render();
    pacfChart.render();
  }

  function resizeSeries(series, newLength, type) {
    const resized = series.slice(0, newLength);
    while (resized.length < newLength) {
      resized.push(generateDefaultSeries(newLength, type)[resized.length]);
    }
    return resized;
  }

  function syncParameter(name, value) {
    state[name] = clamp(parseInt(value, 10) || 0, 0, 10);
    controls[name].value = state[name];
    setControlText();
    updateInfo();
  }

  ["p", "q", "P", "Q"].forEach((name) => {
    controls[name].addEventListener("input", (e) => syncParameter(name, e.target.value));
  });

  controls.acfLags.addEventListener("change", (e) => {
    const lags = clamp(parseInt(e.target.value, 10) || defaults.acfLags, 8, 120);
    state.acfLags = lags;
    controls.acfLags.value = String(lags);
    state.acf = resizeSeries(state.acf, lags, "acf");
    acfChart.setSeries(state.acf);
    updateInfo();
  });

  controls.pacfLags.addEventListener("change", (e) => {
    const lags = clamp(parseInt(e.target.value, 10) || defaults.pacfLags, 8, 120);
    state.pacfLags = lags;
    controls.pacfLags.value = String(lags);
    state.pacf = resizeSeries(state.pacf, lags, "pacf");
    pacfChart.setSeries(state.pacf);
    updateInfo();
  });

  document.querySelector("#resetBtn").addEventListener("click", () => {
    state.p = defaults.p;
    state.q = defaults.q;
    state.P = defaults.P;
    state.Q = defaults.Q;
    state.acfLags = defaults.acfLags;
    state.pacfLags = defaults.pacfLags;
    state.acf = generateDefaultSeries(defaults.acfLags, "acf");
    state.pacf = generateDefaultSeries(defaults.pacfLags, "pacf");

    ["p", "q", "P", "Q"].forEach((name) => {
      controls[name].value = state[name];
    });
    controls.acfLags.value = String(state.acfLags);
    controls.pacfLags.value = String(state.pacfLags);

    acfChart.setSeries(state.acf);
    pacfChart.setSeries(state.pacf);
    setControlText();
    updateInfo();
  });

  setControlText();
  updateInfo();
})();
