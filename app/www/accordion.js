var accordion = document.getElementsByClassName("accordion-container");

function addAccordionListeners() {
  console.log("Setting accordion listeners");
  for (i = 0; i < accordion.length; i++) {
    accordion[i].getElementsByClassName("label")[0].addEventListener("click", function () {
      this.parentElement.classList.toggle("active");
    })
  }
}

function waitForAccordions(callback) {
  console.log("Waiting for accordions...")
  var interval = setInterval(function() {
    var accordion = document.getElementsByClassName("accordion-container");
    if (accordion.length > 0) {
      clearInterval(interval);
      callback();
    }
  }, 250);
}

waitForAccordions(addAccordionListeners);