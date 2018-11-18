document.addEventListener("DOMContentLoaded", function() {
	// style tweaks to parent and sibling elements
	var labels = document.querySelectorAll(".labelledCode");
	for (var i = 0; i < labels.length; i++) {
		var parent = labels[i].parentNode;
		parent.classList.add("plisqin-labelHolder");
		parent.nextSibling.classList.add("plisqin-codeHolder");
	}
});