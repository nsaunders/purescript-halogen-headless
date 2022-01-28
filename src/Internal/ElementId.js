var key = "halogenHeadlessElementId";

exports.elementId = function() {
  if (!window[key]) {
    window[key] = 0;
  }
  return "hhe_" + window[key]++;
};
