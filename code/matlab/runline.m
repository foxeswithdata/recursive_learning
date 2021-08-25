currentEditor = matlab.desktop.editor.getActive;
originalSelection = currentEditor.Selection;
assert(originalSelection(1)==originalSelection(3));%Check that multiple lines are not selected
currentEditor.Selection = [originalSelection(1) 1 originalSelection(1) Inf];%Select the whole line
eval(currentEditor.SelectedText);%Run the whole line
currentEditor.Selection = originalSelection;%Reset selection to original state
