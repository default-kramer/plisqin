if (!Element.prototype.matches) {
  Element.prototype.matches = Element.prototype.msMatchesSelector || 
                              Element.prototype.webkitMatchesSelector;
}

if (!Element.prototype.closest) {
  Element.prototype.closest = function(s) {
    var el = this;

    do {
      if (el.matches(s)) return el;
      el = el.parentElement || el.parentNode;
    } while (el !== null && el.nodeType === 1);
    return null;
  };
}

document.addEventListener("DOMContentLoaded", function(e) {
	var els = document.querySelectorAll(".PShowTable");
	els.forEach(function(el) {
		el.onclick = function() { toggle("show-results", el); }
	});
	els = document.querySelectorAll(".PShowSql");
	els.forEach(function(el) {
		el.onclick = function() { toggle("show-sql", el); }
	});
});

function toggle(cls, el) {
	var el = el.closest(".PTableWrapper");
	el.classList.remove("show-results");
	el.classList.remove("show-sql");
	el.classList.add(cls);
}