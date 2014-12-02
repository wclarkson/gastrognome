var recipeText;
var div;
var nextButton;
var steps = [];
var stepTimer = 0;
var stepTimerDiv;

function formatStep(step) {
  console.log(step);
  return "<b>" + step.action + "</b> " + step.string;
}

function runRecipe() {
  var recipe = JSON.parse(recipeText.value);
  steps = recipe.steps.reverse();
  nextStep();
}

function hideNext() {
  nextButton.disabled = true;
  nextButton.style.display = "none";
}

function showNext() {
  nextButton.disabled = false;
  nextButton.style.display = "inline";
}

function nextStep() {
  if (steps.length == 0) return;

  hideNext();
  var step = steps.pop();
  div.innerHTML = formatStep(step);

  console.log(step);
  stepTimer = step.duration;
  function incrementTimer() {
    if (stepTimer == 0) {
      stepTimerDiv.innerHTML = "";
      if (steps.length == 0) {
        hideNext();
        div.innerHTML = "<em>DONE!</em>";
      } else {
        showNext();
      }
    } else {
      stepTimerDiv.innerText = stepTimer + " s";
      stepTimer -= 1;
      setTimeout(incrementTimer, 1000);
    }
  }
  incrementTimer();
}

function init() {
  recipeText = document.getElementById("json");
  div = document.getElementById("recipe");
  nextButton = document.getElementById("nextStep");
  stepTimerDiv = document.getElementById("nextStepTimer");

  document.getElementById("runRecipe").addEventListener("click", runRecipe);
  nextButton.addEventListener("click", nextStep);
  nextButton.disabled = true;
}

window.onload = init;
