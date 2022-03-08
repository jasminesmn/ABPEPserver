function openCancer(evt, cancerName) {
    // Declare all variables
    var i, tabcontent, tablinks;
    
    // Get all elements with class="tabcontent_cancers" and hide them
    tabcontent = document.getElementsByClassName("tabcontent_cancers");
    for (i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = "none";
    }
    
    // Get all elements with class="tablinks" and remove the class "active"
    tablinks = document.getElementsByClassName("tablinks_cancers");
    for (i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(" active", "");
    }
    
    // Show the current tab, and add an "active" class to the link that opened the tab
    document.getElementById(cancerName).style.display = "block";
    evt.currentTarget.className += " active";

}