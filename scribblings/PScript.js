document.addEventListener("DOMContentLoaded", function(e) {
	// Stick some radio buttons ("togglers") into the PLangToggle placeholders
	var els = document.querySelectorAll(".PLangToggle");
	for(var i = 0; i < els.length; i++) {
		els[i].innerHTML = "#lang"
		+ "<input class='toggler' type='radio' name='lang-toggle"+i+"' id='lang-toggle-plisqin"+i+"' value='plisqin'>"
		+ "<label for='lang-toggle-plisqin"+i+"'>plisqin</label>"
		+ "<input class='toggler' type='radio' name='lang-toggle"+i+"' id='lang-toggle-racket"+i+"' value='racket'>"
		+ "<label for='lang-toggle-racket"+i+"'>racket</label>";
	}
	
	function toggleLang(lang) {
		// lang should be "plisqin" or "racket"
		document.body.className = lang;
		var els = document.querySelectorAll(".toggler");
		for(var i = 0; i < els.length; i++) {
			els[i].checked = (els[i].value == lang);
		}
	}
	
	toggleLang("plisqin");
	
	els = document.querySelectorAll(".toggler");
	for(var i = 0; i < els.length; i++) {
		els[i].addEventListener('change', function(e) {
			if(e.target && e.target.checked && e.target.value) {
				toggleLang(e.target.value);
			}
		});
	}
});