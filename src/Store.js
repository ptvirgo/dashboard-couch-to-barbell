"use strict";

const saveName = "dashboardCouchToBarbell"

export const saveIt = function(theThing) {
    localStorage.setItem(saveName, JSON.stringify(theThing));
    console.debug(theThing);
}

export const loadIt = function() {
    theThing = localStorage.getItem(saveName) ? JSON.parse(localStorage.getItem(saveName)) : null;
    console.debug(theThing);
    return(theThing);
}
