shinyjs.contextmenu = function(){
    // Variables
	var taskItemInContext;
	
	var menu = document.querySelector(".context-menu");
	var menuState = 0;
	
	var activeClassName = "context-menu--active";
	var contextMenuItemClassName = "context-menu__item";
	var contextMenuLinkClassName = "context-menu__link";
	var taskItemClassName = 'download';
	
	///////////////////////////////////////
	///////////////////////////////////////
	//
	// H E L P E R    F U N C T I O N S
	//
	///////////////////////////////////////
	///////////////////////////////////////
	
	// show the contextmenu
	function toggleMenuOn() {
		if ( menuState !== 1 ) {
			menuState = 1;
			menu.classList.add(activeClassName);
		}
	}
	
	// hide the contextmenu
	function toggleMenuOff() {
		if ( menuState !== 0 ) {
			menuState = 0;
			menu.classList.remove(activeClassName);
		}
	}
	
	function clickInsideElement( ev, className ) {
		var el = ev.srcElement || ev.target;

		if ( el.classList.contains(className) ) {
			return el;
		} else {
			while ( el = el.parentNode ) {
				if ( el.classList && el.classList.contains(className) ) {
					return el;
				}
			}
		}

		return false;
	}	
	
	// get position of event
	function getPosition(ev) {
		var posx = 0;
		var posy = 0;

		if (!ev) var ev = window.event;
	
		if (ev.pageX || ev.pageY) {
			posx = ev.pageX;
			posy = ev.pageY;
		} else if (ev.clientX || ev.clientY) {
			posx = ev.clientX + document.body.scrollLeft + 
                       document.documentElement.scrollLeft;
			posy = ev.clientY + document.body.scrollTop + 
                       document.documentElement.scrollTop;
		}

		return {
			x: posx,
			y: posy
		}
	}
	function positionMenu(ev) {
		menuPosition = getPosition(ev);
		menuPositionX = menuPosition.x + "px";
		menuPositionY = menuPosition.y + "px";

		menu.style.left = menuPositionX;
		menu.style.top = menuPositionY;
	}
	
	///////////////////////////////////////
    ///////////////////////////////////////
    //
	// C O R E    F U N C T I O N S
	//
	///////////////////////////////////////
	///////////////////////////////////////
	
	function init(){
		contextListener();
		clickListener();
	}
	
	// global right click listener
	function contextListener() {
		document.addEventListener("contextmenu", function(ev){
			taskItemInContext = clickInsideElement( ev, taskItemClassName );
			if(taskItemInContext){
				ev.preventDefault();
				toggleMenuOn();
				positionMenu(ev);
			} else {
				taskItemInContext = null;
				toggleMenuOff();
			}	
		});
	}
	
	// global left click listener
	function clickListener() {
		document.addEventListener( "click", function(e) {
			var clickeElIsLink = clickInsideElement( e, contextMenuLinkClassName );
			
			if(clickeElIsLink){
				e.preventDefault();
				menuItemListener( clickeElIsLink );
				toggleMenuOff();
			} else {
				var button = e.which || e.button;
				if ( button === 1 ) {
					toggleMenuOff();
				}	
			}			
		});
	}
	
	function menuItemListener( link ) {
		var lnk = document.querySelector("#downloadHelper").getAttribute("href")
		var message = {plot: taskItemInContext.getAttribute("id"), action: link.getAttribute("plot-action"), lnk: lnk, nonce: Math.random()};
		Shiny.onInputChange("right_click", message);	
		console.log( "Task ID - " + 
                taskItemInContext.getAttribute("id") + 
                ", Task action - " + link.getAttribute("plot-action"));
	}
	
	
	init();
}