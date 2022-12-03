var key = "halogenHeadlessElementId";

export function elementId() {
  if (!window[key]) {
    window[key] = Number.MIN_SAFE_INTEGER;
  }
  return "hhe_" + window[key]++;
};
