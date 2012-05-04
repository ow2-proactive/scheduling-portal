if (window.addEventListener) {
	var keys = [];
	var ka = [ 38, 38, 40, 40, 37, 39, 37, 39, 66, 65 ]
	var i = 0;
	window.addEventListener("keydown", function(e) {
		if (e.keyCode === ka[i]) {
			keys.push(e.keyCode);
			i += 1;
			if (ka.length === keys.length) {
				alert('OK');
				keys = [];
				i = 0;
			}
		} else {
			keys = [];
			i = 0;
		}
		console.log(keys);
	}, true);
}