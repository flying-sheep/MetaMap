shinyjs.init = function(){
	// 1) hide analysis tabs
	$("#study_title").hide();
	$("#dataset li a[data-value='Define sample grouping']").hide();
	$("#dataset li a[data-value='Analysis']").hide();
	$(".hidden_tab").hide();
	$("#back_button").prop( "disabled", true );
	$("#download_samples").prop( "disabled", true );
	
	// 2) unfocus buttons after click
	$('.btn').mouseup(function() { this.blur() });
	
	// 3) show custom contextenu on right click on plots (download option)
	// Source first contextmenu.js
	shinyjs.contextmenu();
	
	// 4) Define downloadable plots
	$("#mfPlot").addClass("download")
	$("#dimred").addClass("download")
	$("#diversity").addClass("download")
	$("#top_species_plot").addClass("download")
	$("#de_plot").addClass("download")
	$("#de_boxplot").addClass("download")
	$("#taxa_plot").addClass("download")
	$("#ntaxa_plot").addClass("download")
} 

// function to reset the click value of sankey plot
shinyjs.resetClick = function() { 
	Shiny.onInputChange('.clientValue-plotly_click-sankey', 'null'); 
}

// write krona file
shinyjs.writeKrona = function(input){
	var doc = document.getElementById('krona-file').contentWindow.document; 
	console.log("works");
	doc.open(); 
	doc.write(input); 
	doc.close();
}
