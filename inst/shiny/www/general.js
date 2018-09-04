shinyjs.init = function(){
	// 1) hide analysis tabs
	$("#dataset li a[data-value='<div id=\"study_title\" class=\"shiny-html-output\"></div>").hide();
	$("#dataset li a[data-value='Customize Data']").hide();
	$("#dataset li a[data-value='Analysis']").hide();
	$(".hidden_tab").hide();
	$("#fwd_button").prop( "disabled", true );
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
	$("#cor_plot").addClass("download")
} 

// function to reset the click value of sankey plot
shinyjs.resetClick = function() { 
	Shiny.onInputChange('.clientValue-plotly_click-sankey', 'null'); 
}

// write krona file
shinyjs.writeKrona = function(input){
	krona = document.getElementById('krona-file');
	if(krona == null){
		return null;
	}
	var doc = krona.contentWindow.document;
	doc.open(); 
	doc.write(input); 
	doc.close();
}
