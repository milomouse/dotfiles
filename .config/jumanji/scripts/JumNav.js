// JumNav.js
// Version 1.0
//
// Autor: Bernd Pol <bernd.pol@online.de>
// License: GPL
//
// Navigation script for use with jumanji
// (jumanji-git >= 20100717 & patched for JumNav use)

/****************************
 *** Known Bugs / Caveats ***
 ****************************/
/*
 * - There are still pages where the hinting labels will not be properly 
 *   positioned initially. In most cases they can made to "snap in" if
 *   you switch into navigation mode and toggle raw mode on and off again.
 *   These positions will be kept after switching back to hinting mode.
 * 
 * - The automatic viewport adjust to the selected element does not work
 *   on a few complicated structured pages. In many such cases the next/
 *   previous navigation helps to make the selcted element visible again.
 *
 * - On pages with a real lot of linkable elements JumNav will not respond
 *   immediately on initial call. It can take several seconds (example: up to
 *   10 seconds expired on a page sporting 2137 links using a 2 GHz Athlon) 
 *   until the hints will be shown. 
 *   Currently JumNav cannot notify the user about the hinting labels building 
 *   progress, so if in doubt about a page please be patient.
 * 
 * - In word matching mode there is only a reduced regular expression pattern
 *   possible because the jumnav global buffer filters some non-alphanumeric
 *   characters out.
 */

/*****************
 *** Changelog ***
 *****************/
/*
 * Version 1.0
 * - Recognizes now anchor elements with no size by themselves but bound 
 *   to image childs.
 * - The selected element can be "clicked on" in any mode
 *   (command clickNodeChar, defaults to ",")
 * - Word match mode implemented as sub-mode to navigation mode
 *   using commands matchOnChar, default "/",
 *                  matchOffChar, default "="
 * - When in matching mode, (default) "#" toggles between navigation and 
 *   word matching, otherwise "#" toggles between navigation and hinting.
 *
 * Version 0.4
 * - More element types will be handled properly if selected ("clicked").
 * - Positioning algorithm reworked.
 *   Labels and element links will now be (almost) correctly placed on
 *   most pages.
 * - Element links will be positioned in the last selected hinting group.
 * - Element links keep their position when hinting mode was toggled on and
 *   off again.
 * - Viewport follows current element link.
 * - Vieport follows hinting group if all elements of it were outside
 *   when the group has been partly selected.
 * - Element link colors optimized.
 * - Numbers can be excluded from optimal hint label strings 
 *   ("useNumbersInOptimal" option).
 * - Contents section added (below).
 *
 * Version 0.3
 * - Bug fixed: Handles most links properly now, esp. javascript calls 
 *              on the page
 * - Navigation mode implemented: allows to navigate back and forth the
 *              clickable elements hierarchy on the page instead of 
 *              following hinting labels.
 *              Also allows for some interaction (display mode changes).
 *              Defaults:
 *              #   toggle navigation/hinting mode
 *              n   go to next node in the hierarchy
 *              p   go to previous node in the hierarchy
 *              s   select ("click") the current node
 *              +   toggle display colors
 *              -   toggle a "raw" position mode in order to properly
 *                  align the hinting labels/node rectangles on some pages
 *
 * Version 0.2
 * - If an action was performed by this script on a selection,
 *   i.e. a mouse click simulation, jumanji could not be informed
 *   about this and would thus not get out of the follow mode:
 *   --> JumNav now returns a special notification to Jumanji
 *       Note: This requires Jumanji being able to process this value upon 
 *             return from the update_hints() call.
 *             Otherwise the "resetNormalNotification" configuration variable
 *             must be set to an empty string to not have Jumanji this value
 *             interpreted as an URL.
 *
 * - Bug fixed: hints with 1 label only were not processed.
 */

/****************
 *** Contents ***
 ****************/
/*
 * Configuration
 *   --- User Configuration Section ---
 *   Collateral Sequences
 *      var collSequence
 *      var useNumbersInOptimal
 *   Simulate a Mouse Click
 *      var clickNodeChar
 *   Node label Display
 *      var shortenLabels
 *      var nodeLabelSize
 *   Hinting labels display
 *      var nodeLabelBorderWidth
 *      var nodeLabelBorder
 *      var nodeLabelColor
 *      var nodeLabelBackground
 *      var partialLabelColor
 *      var partialLabelBackground
 *      var foundLabelColor
 *      var foundLabelBackground
 *      var nodeOpacity
 *   Navigatable Elements Display
 *      var navElementBorderWidth
 *      var navElementBorder
 *      var navElementBackground
 *      var curNavElementBorderWidth
 *      var curNavElementBorder
 *      var curNavElementBackground
 *      var navElementOpacity
 *   Navigation Mode Commands
 *      Navigation mode switch
 *          var modeSwitchChar
 *          var matchOnChar
 *          var matchOffChar
 *          var displaySwitchChar
 *          var switchRawChar
 *          var nextNodeChar
 *          var previousNodeChar
 *   --- End of User Configuration Section ---
 *   The overlay identification
 *      var overlayId
 *   Click processing notification
 *      var clearBufferNotification
 *      var resetNormalNotification
 *
 * Initialization
 *   Common Variables
 *      Clickables Database
 *          var clickableNodes
 *          var elementsOverlays
 *          var labelsOverlays
 *          var matchingOverlays
 *          var nodeLabels
 *      Navigating
 *          var hasLinkNodes
 *          var curLabelHead
 *          var lastLabelHead
 *          var curMatchingPattern
 *          var lastMatchingPattern
 *          var curLabelNum
 *          var matchingLabelNum
 *          var inMatchingMode
 *          var labelDigits
 *      Collateral Sequences
 *          var useSequence
 *          var useBase
 *      Navigation Control
 *          var navigationMode
 *          var curNavCommand
 *          var displayMode
 *      Hacks
 *          var rawMode
 *
 * Initialization
 *      initializeAll()
 *      clearLinkInfo()
 *      initCollSequence()
 *
 * Miscellaneous Stuff
 *   Label Handling
 *          labelTextOf( posNumber )
 *          labelNumberOf( labelString )      // (disabled)
 *   Maintaining Navigation Information
 *          isVisible( thisElement )
 *          isDisplayable( thisElement )
 *          isClickable( thisElement )
 *          positionOf( thisElement, horOffset, vertOffset, horCorr, vertCorr )
 *          absolutePositionOf( thisElement )
 *          viewportPosition()
 *          inViewport( thisElement )
 *          isValidLabel( thisHead )
 *   Access Actions
 *          emulateClickOn( curElement )
 *          clickNode( elementNr )
 *   Build Hints Database
 *          findClickableNodes()
 *          createOverlays()
 *
 * Hinting Based Navigation
 *   Hinting Labels Display
 *          switchLabelsDisplay()
 *          showLabelOverlays( labelHead )
 *          hideLabelOverlays()
 *          redisplayOverlays()         // (disabled)
 *          removeOverlays()
 *
 * Element Links Based Navigation
 *   Element Links Display
 *          switchElementsDisplay()
 *          showElementOverlays()
 *          showMatchingOverlays()
 *          hideElementOverlays()
 *          goNextNode()
 *          goPreviousNode()
 *   Navigation Commons
 *          enhanceElementDisplay( elementNum )
 *          normalElementDisplay( elementNum )
 *          adjustViewportFor( thisElement )
 *          switchRawMode()
 *          switchDisplayMode()
*           useHintingMode()
*           useNavigationMode()
 * 
 * Jumanji Main Interface
 *      show_hints()
 *      update_hints( labelHead )
 *      clear()
 */

/*********************
 *** Configuration ***
 *********************/
// -----------------------------------------------------------------------------
//                        start of user configuration section
// -----------------------------------------------------------------------------
/*
 * Collateral Sequences
 * ====================
 * 
 * There are several label number representations possible. Just uncomment the
 * one you want.
 *
 * Note that the first symbol in sequence will be treated as zero equivalent
 * and the labels will be get those zero equivalents prepended if necessary,
 * e.g. the number 1 in a three-digit "alpha" sequence will show as "aab".
 */
var collSequence = "optimal";   // automatic: find shortest to type sequence

// var collSequence = "numeric";    // decimal numbers
// var collSequence = "alpha";      // lower case letter sequences
// var collSequence = "longalpha";  // lower followed by upper case letters

/*
 * Alternatively this can be any unique sequence of alphanumeric symbols, e.g.:
 */
// var collSequence = "asdfghjkl";  // home row keys (for touch typers)
// var collSequence = "uiophjklnm"; // right hand only
// var collSequence = "01";         // scary but possible: binary only
/*
 * True if the computing of an optimal collateral sequence shall consider 
 * numbers as well. Otherwise only letters will bu used.
 */
var useNumbersInOptimal = false;
/*
 * Simulate a Mouse Click
 * ======================
 * This causes the currently selected node being "clicked on".
 * Use a character which is not likely to occur in word match regular
 * expressions nor being part of a label.
 */
var clickNodeChar = ",";
/*
 * Node label display
 * ==================
 */
/*
 * If true show matching labels and selectable digits only.
 * If false keep the labels intact and mark the selectable group in an
 * alternative color (see below: partialLabelColor[displayMode], partialLabelBackground[displayMode]).
 */
var shortenLabels = true;
/*
 * This defines the font size shown in the labels. It may be an absolute number
 * with a trailing "px" giving the font height in pixels, a number with trailing
 * "%" giving the height relative to the parents font size, or one of the
 * predefined font size property values (ranging from smallest to largest):
 * "xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large"
 * or defining a relative value to the parent font size:
 * "smaller", "larger"
 * ( see also: http://www.w3schools.com/jsref/prop_style_fontsize.asp )
 */
var nodeLabelSize = "10px";
// var nodeLabelSize = "85%";
// var nodeLabelSize = "small";
/*
 * Hinting labels display
 * ----------------------
 * There are two sets of colors so you can switch displays if they become 
 * unreadable on a certain side.
 * Use color names or "#rrggbb" hexadecimal values for red, green, and blue
 * channels respectively.
 * Background colors may be set to "transparent" if the inherited background
 * color shall be used.
 */
var nodeLabelBorderWidth = [1, 1];
var nodeLabelBorder = ["black", "yellow"];
var nodeLabelBorderStyle = ["solid", "solid"];

var nodeLabelColor = ["darkred", "white"];
var nodeLabelBackground = ["yellow","black"];
/*
 * How to display the partial selection group if shortenLabels was set to false.
 */
var partialLabelColor = ["yellow", "yellow"];
var partialLabelBackground = ["green", "darkblue"];
/*
 * How to display the finally selectable label
 * (in case the user needs to confirm the selection).
 */
var foundLabelColor = ["yellow", "yellow"];
var foundLabelBackground = ["red", "brown"];
/*
 * Define the transparence of the labels.
 */
var nodeOpacity = 0.6;
/*
 * Navigatable Elements Display
 * ----------------------------
 * Use color names or "#rrggbb" hexadecimal values for red, green, and blue
 * channels respectively.
 */
var navElementBorderWidth = [1, 1];
var navElementBorder = ["darkblue", "yellow"];
var navElementBorderStyle = ["dashed", "dashed"];
var navElementBackground = ["yellow", "blue"];
var matchElementBackground = ["red", "green"]; // use in matching mode
/*
 * How to display the currently selected element.
 */
var curNavElementBorderWidth = [1, 1];
var curNavElementBorder = ["darkred", "darkgreen"];
var curNavElementBorderStyle = ["dashed", "dashed"];
var curNavElementBackground = ["green", "red"];
/*
 * Define the transparence of the selectable elements display.
 */
var navElementOpacity = 0.4;
/*
 * ========================
 * Navigation Mode Commands
 * ========================
 */
/*
 * Navigation mode switch
 * ----------------------
 * If JumNav finds this mode switch command character as most recent input in
 * the label head string when update_hints was called while running in hinted 
 * mode, it will switch to navigation mode. When in navigation mode it will
 * switch back to hinted mode.
 * Word matching navigation is a sub-mode of this. As long as it is active,
 * navigation will occur in the matching element links group only.
 */
var modeSwitchChar = '#';       // toggle hinting/matching with navigation mode
                                // (use a char not very likely in a pattern)

var matchOnChar = '/';          // enable word match navigation
var matchOffChar = '=';         // disable word match navigation
                                // (use a char not very likely in a pattern)

var displaySwitchChar = "+";    // toggle node/label display colors
/*
 * This switches the positions of the elements to adjust to for some 
 * (presumedly) webkit element position reporting problem.
 * Use this if the hints appear misaligned on the current page (switch to
 * navigation mode, then switch to raw mode, and then back again to hinting
 * mode if wanted).
 * This is a toggle. It effectively re-creates the hints and labels to be 
 * displayed on the page.
 */
var switchRawChar = "-";
/*
 * Go to next/previous node
 */
var nextNodeChar = "n";
var previousNodeChar = "p";

// -----------------------------------------------------------------------------
//                        end of user configuration section
// -----------------------------------------------------------------------------
/*
 * The overlay identification
 * --------------------------
 * This will be prepended to every label overlay element. Redefine if there are
 * name conflicts.
 */
var overlayId = "JumNavLabel";
/*
 * Click processing notification
 * -----------------------------
 * These are values JumNav returns to Jumanji when it has processed a link by
 * its own, i.e. usually some mouseclick emulation, or simply wants Jumanji to
 * clear its global buffer.
 * It requires that Jumaji can process these values. Otherwise set these 
 * notifications to empty strings.
 */
var clearBufferNotification = "\\";
var resetNormalNotification = "\\\\";

/**********************
 *** Initialization ***
 **********************/
/*
 * Common Variables
 * ================
 */
/*
 * Clickables Database
 * -------------------
 */
var clickableNodes;
var elementsOverlays;
var matchingOverlays;
var labelsOverlays;
var nodeLabels;
/* 
 * Navigating
 * ----------
 */
var hasLinkNodes;
var curLabelHead;
var lastLabelHead;
var curMatchingPattern;
var lastMatchingPattern;
var curLabelNum;
var matchingLabelNum;
var inMatchingMode;
var labelDigits;
/* 
 * Collateral Sequences
 * --------------------
 */
var useSequence;
var useBase;
/*
 * Navigation Control
 * ------------------
 */
var navigationMode = false;
var curNavCommand;
var displayMode;
/*
 * Hacks
 * -----
 */
var rawMode;
/*
 * Initialize All
 * ==============
 */
function initializeAll() {
    initCollSequence();
    clearLinkInfo();
    findClickableNodes();
    if (hasLinkNodes) {
        createOverlays();
    }
}
/*
 * Clear All Link Information
 * ==========================
 */
function clearLinkInfo() {
    removeOverlays();
    elementsOverlays = null;
    labelsOverlays = null;
    matchingOverlays = null;
    nodeLabels = null;
    clickableNodes = null;
    hasLinkNodes = false;
    curLabelHead = "";
    lastLabelHead = "";
    curMatchingPattern = "";
    lastMatchingPattern = "";
    curLabelNum = -1;           // marks number as invalid
    matchingLabelNum = -1;
    inMatchingMode = false;
    labelDigits = 0;
    navigationMode = false;
    curNavCommand = "";
    displayMode = 0;
    rawMode = false;
}
/*
 * Initialize Collation Sequences
 * ==============================
 */
function initCollSequence() {
    if (collSequence.length <= 1) {
        // invalid sequence: default to numbers
        useSequence = "0123456789";
        useBase = 10;
    } 
    else if (collSequence == "numeric") {
        useSequence = "0123456789";
        useBase = 10;
    } 
    else if (collSequence == "alpha") {
        useSequence = "abcdefghijklmnopqrstuvxyz";
        useBase = 25;
    } 
    else if (collSequence == "longalpha") {
        // We use lower key characters first, then upper key ones
        // to ease the typing.
        useSequence = "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ";
        useBase = 50;
    } 
    else if (collSequence != "optimal") {
        useSequence = collSequence;
        useBase = collSequence.length;
    }
}

/***************************
 *** Miscellaneous Stuff ***
 ***************************/
/*
 * Label Handling
 * ==============
 */
/*
 * Construct the label text for a given position number
 * ----------------------------------------------------
 * @param posNumber decimal position number
 *                  (must be >= 0)
 * @return          string representation of this number according to the
 *                  predefined collateral sequence.
 *                  Leading filled with the zero equivalence of the predefined
 *                  collateral sequence up to labelDigits length.
 */
function labelTextOf( posNumber ) {
    var head = posNumber;
    var remainder = 0;
    var labelString = "";
    /*
     * Numeric sequences should count from 1 instead from 0.
     */
    if (collSequence == "numeric" ||
        (collSequence == "optimal" && useSequence.charAt(0) == "0"))
        head++;
    /*
     * Compute the symbolic digits.
     */
    if (head == 0) {
        labelString = useSequence.charAt(0);
    }
    while (head > 0) {
        remainder = head % useBase;
        labelString = useSequence.charAt(remainder) + labelString;
        head = (head - remainder) / useBase;
    }
    // Fill with the zero equivalent of this collateral sequence.
    while (labelString.length < labelDigits) {
        labelString = useSequence.charAt(0) + labelString;
    }
    return labelString;
}
/*
 * Construct the label number for a given label string
 * ---------------------------------------------------
 * @param labelString   string representation of the label
 * @return              decimal equivalent according to the prdefined
 *                      collataration sequence.
 */
//function labelNumberOf( labelString ) {
//    var posNumber = 0;
//    var curBase = useBase;
//    var curDigit;
//
//    for (var i=labelString.length-1; i >= 0; i--) {
//        curDigit = labelString.charAt(i);
//        posNumber += useBase * useSequence.indexOf(curDigit);
//        curBase *= useBase;
//    }
//    /*
//     * Adjust for numeric counting from 1 instead of 0.
//     */
//    if (collSequence == "numeric" ||
//        (collSequence == "optimal" && useSequence.charAt(0) == "0"))
//        posNumber--;
//
//    return posNumber;
//}
/*
 * Maintaining Navigation Information
 * ==================================
 */
/*
 * Check visibility of an element
 * ------------------------------
 * Recursively checks the given Element wether it is not hidden.
 *
 * @param   thisElement  node to be checked.
 * @return  true if this Element is not hidden and not behind a hidden parent in
 *          the DOM tree.
 */
function isVisible( thisElement ) {
    if ( thisElement == document ) {
        return true;
    }
    if ( ! thisElement ) {
        return false;
    }
    if ( ! thisElement.parentNode ) {
        return false;
    }
    if ( thisElement.style ) {
        if ( thisElement.style.display == 'none' ) {
            return false;
        }
        if ( thisElement.style.visibility == 'hidden' ) {
            return false;
        }
    }
    return isVisible( thisElement.parentNode );
}
/*
 * Check if the element is displayable at all
 * ------------------------------------------
 * @param   thisElement element to check
 * @return  true if it can be displayed
 */
function isDisplayable( thisElement ) {
    var width = thisElement.offsetWidth;
    var height = thisElement.offsetHeight;

    if (width <= 0 || height <= 0) {
        /*
         * An element occupying no space usually will be not displayable.
         * It can be an anchor though, associated with an image.
         */
        if (thisElement.nodeName.toLowerCase() == "a") {
            /*
             * This node needs to be clickable of course.
             */
            if (! isClickable( thisElement ) )
                return false;
            /*
             * If so check whether there is an image child.
             */
            for (var i = 0; i < thisElement.childNodes.length; i++) { 
                if (thisElement.childNodes[i].nodeName.toLowerCase() == "img")
                    return true;
            }
        }
        else {
            return false;
        }
    }
    else 
        return true;
}
/*
 * Check if the element is clickable
 * ---------------------------------
 * @param   thisElement element to check
 * @return  true if it can clicked on
 */
function isClickable( thisElement ) {
    /*
     * Anchors must refer to something.
     */
    if (thisElement.nodeName.toLowerCase() == "a" &&
        (thisElement.hasAttribute( "href" ) || thisElement.hasAttribute( "onclick" ) ) )
        return true;

    if ( thisElement.nodeName.toLowerCase() == "input" ||
         thisElement.nodeName.toLowerCase() == "select" ||
         thisElement.nodeName.toLowerCase() == "textarea" ||
         thisElement.hasAttribute( "tabindex" ) ||
         thisElement.hasAttribute( "href" ) ||
         thisElement.hasAttribute( "onclick" ) )
        return true;
    else
        return false;
}
/*
 * Evaluate the position of an element
 * -----------------------------------
 * Computes the position of the element rectangle on the page.
 * Note: This position can deviate from the raw (inherited (?)) position of
 *       the node this element overlay was attached to.
 *
 *  @param  thisElement element to inspect
 *          horOffset   number of pixels to shift the element left
 *          vertOffset  number of pixels to shift the element up
 *          horCorr     number of pixels to add if at left border
 *          vertCorr    number of pixels to add if at top border
 *
 *  @return array [top, left, width, height] of position and size
 */
function positionOf( thisElement, horOffset, vertOffset, horCorr, vertCorr ) {
    return getPositionOf( thisElement, horOffset, vertOffset, horCorr, vertCorr, false );
}
/*
 * Adjustable evaluation of the position of an element
 * ---------------------------------------------------
 * Computes the position of the element rectangle on the page.
 * Note: This position can deviate from the raw (inherited (?)) position of
 *       the node this element overlay was attached to.
 *
 * This rather involved algorithm is due to difficulties to obtain the correct
 * position of an element on some pages with a rather complex structure.
 * It has been merely derived by trial and error and is still far from been
 * really understood. Obviously some hinting labels and element nodes will be
 * misplaced if the whole parent tree is taken into account. On the other hand
 * we need to sum up all offsets if we want to know where an object is placed
 * on the page (i.e. for viewport adjustment calculations).
 *
 *  @param  thisElement element to inspect
 *          horOffset   number of pixels to shift the element left
 *          vertOffset  number of pixels to shift the element up
 *          horCorr     number of pixels to add if at left border
 *          vertCorr    number of pixels to add if at top border
 *          fullAdjust  true if all parent offsets shall be summed up
 *                      false to leave some out which tend to double count
 *
 *  @return array [top, left, width, height] of position and size
 */
function getPositionOf( thisElement, horOffset, vertOffset, horCorr, vertCorr, fullAdjust ) {
    var thisName;

    var upper = thisElement.offsetTop;
    var left = thisElement.offsetLeft;
    var width = thisElement.offsetWidth;
    var height = thisElement.offsetHeight;

    if (width <= 0 || height <= 0) {
        /*
         * This element has no actual size given.
         * But if it is associated with an image, use the dimensions there.
         */
        for (var i = 0; i < thisElement.childNodes.length; i++) { 
            if (thisElement.childNodes[i].nodeName.toLowerCase() == "img") {
                width = thisElement.childNodes[i].offsetWidth;
                height = thisElement.childNodes[i].offsetHeight;
                break;
            }
        }
    }

    while (thisElement.offsetParent) {
        thisElement = thisElement.offsetParent;
        thisName = thisElement.nodeName.toLowerCase();
        if (fullAdjust ||
            (thisName != "div" && thisName != "fieldset" && thisName != "li") ) {
            upper += thisElement.offsetTop;
            left += thisElement.offsetLeft;
        }
    }

    upper -= vertOffset;
    left -= horOffset;

    if (horCorr != 0)
        if (left < horCorr)
            left = horCorr;

    if (vertCorr != 0)
        if (upper < vertCorr)
            upper = vertCorr;

    return [upper, left, width, height];
}
/*
 * Get the page position of an element
 * -----------------------------------
 * @param   thisElement     element whose position is to be found
 * @return  array [top, left, right, bottom] of the element rectangle
 */
function absolutePositionOf( thisElement ) {
    var elPos = getPositionOf( thisElement, 0, 0, 0, 0, true );

    elPos[2] = elPos[1] + elPos[2];     // element right edge
    elPos[3] = elPos[0] + elPos[3];     // element bottom edge

    return elPos;
}
/*
 * Get the viewport position
 * -------------------------
 * @return  array [top, left, right, bottom] of the viewport borders
 */
function viewportPosition() {
    var winPos = [window.pageYOffset,                       // top border
                  window.pageXOffset,                       // left border
                  window.pageXOffset + window.innerWidth,   // right border
                  window.pageYOffset + window.innerHeight]; // bottom border

    return winPos;
}
/*
 * Check which part of an element lies within the viewport
 * -------------------------------------------------------
 * @param   thisElement element to be checked
 * @return  0 if the element is fully visible
 *         -1 if no part of the element is visible
 *          1 if the top edge is outside
 *          2 if the left edge is outside
 *          4 if the right edge is outside
 *          8 if the bottom edge is outside
 *          or the sum of 1..8 if applicable
 */
function inViewport( thisElement ) {
    var elPos = absolutePositionOf( thisElement );
    var winPos = viewportPosition();
    var outside = 0;

    if (elPos[0] > winPos[0] && elPos[1] > winPos[1] &&
        elPos[2] < winPos[2] && elPos[3] < winPos[3])
            return 0;

    if (elPos[0] >= winPos[3] || elPos[1] >= winPos[2] ||
        elPos[2] <= winPos[1] || elPos[3] <= winPos[0])
            return -1;

    if (elPos[0] < winPos[0]) outside  = 1;
    if (elPos[1] < winPos[1]) outside += 2;
    if (elPos[2] > winPos[2]) outside += 4;
    if (elPos[3] > winPos[3]) outside += 8;

    return outside;
}
/*
 * Check if the label is valid
 * ---------------------------
 * Checks if a label starting with the given digits sequence is known in the
 * nodeLabels array.
 *
 * @param   thisHead    head sequence of the label to check
 * @return  true        there are labels starting with this sequence
 *          false       there are no such labels known
 */
function isValidLabel( thisHead ) {
    if (thisHead == "")
        return true;
    /*
     * Try to find a matching labels group.
     */
    var headLength = thisHead.length;
    for( var i = 0; i < nodeLabels.length; i++ ) {
        if (thisHead == nodeLabels[i].substring( 0, headLength )) 
            return true;
    }
    return false;
}
/*
 * Adjust label head to a given element
 * ------------------------------------
 * @param   thisElementNr   number of the element to adjust the group to
 * @return  if the group has to be changed: lastLabelHead set to this group start
 *          else no change
 */
function adjustLabelHeadTo( thisElementNum ) {
    var useHeadLength = curLabelHead.length;
    var foundLabelHead;

    if (useHeadLength == 0)
        return;

    foundLabelHead = nodeLabels[thisElementNum].substring( 0, useHeadLength );
    if (foundLabelHead != curLabelHead) {
        lastLabelHead = foundLabelHead;
    }
}
/*
 * Access Actions
 * ==============
/*
 * Emulate a mouse click
 * ---------------------
 * @param   curElement  element to click on
 */
function emulateClickOn( curElement ) {
    /*
     * This requires some more effort in order to trigger the attached
     * actions.
     * At first we need a special event to track mouse clicks.
     */
    var thisEvent = document.createEvent("MouseEvents");
    /*
     * Then the mouse click action needs to be defined.
     */
    thisEvent.initMouseEvent(
        "click",        // the event type
        true, true,     // allow bubbles and default action cancels
        window,         // this view's base
        0,              // mouse click count
        0, 0, 0, 0,     // screen and client coordinates
        false, false,   // no control or alt key depressed simultaneously
        false, false,   // ditto, shift or meta key
        0,              // mouse button
        null);          // no other related target
    /*
     * Finally get this known to the system.
     */
    curElement.dispatchEvent(thisEvent);
}
/*
 * Perform a mouse click on an element
 * -----------------------------------
 *
 *  @param  elementNr    number of the label to be clicked on
 */
function clickNode( elementNr ) {
    var curElement = clickableNodes[ elementNr ];
    var curLabel = nodeLabels[ elementNr ];
    var curName = curElement.nodeName.toLowerCase();

    clear();
    /*
     * Anchor
     * ------
     */
    if (curName == "a") {
        /*
         * It is a link. Go there if possible.
         */
        var thisReference = curElement.href;    // get full address (javascript!)
        /*
         * There are a few special cases to consider.
         */
        if (thisReference.toLowerCase().match(/javascript:/)) {
            /*
             * Javascript commands have highest priority.
             */
            emulateClickOn( curElement );
        }
        else if (curElement.hasAttribute("onclick")) {
            /*
             * Then if there is an onclick action this must be handled even if
             * there is a href link whose information is most likely to be used
             * by the onclick routine instead of meant to be called directly.
             */
            emulateClickOn( curElement );
        }
        else {
            return thisReference;
        }
    }
    /*
     * OnClick
     * -------
     */
    else if (curElement.hasAttribute("onclick")) {
        emulateClickOn( curElement );
    } 
    /*
     * Input
     * -----
     */
    else if (curName == "input") {
        /*
         * There are several types of input elements which need be handled
         * differently.
         */
        var curType = curElement.getAttribute('type').toLowerCase();

        if (curType == 'text' || curType == 'file' || curType == 'password') {
            /*
             * These need be explicitely selected.
             */
            curElement.focus();
            curElement.select();
            curElement.click();
        }
        else if (curType == "radio" || curType == "checkbox") {
            /*
             * Just toggle these.
             */
            curElement.checked = ! curElement.checked;
        } else {
            /*
             * It is a genuine input element. 
             * This allows us to use the click() method.
             */
            curElement.click();
        }
    }
    /*
     * Special Input Elements
     * ----------------------
     */
    else if (curName == 'textarea' || curName == 'select') {
        /*
         * Handle these like the special input element types.
         */
        curElement.focus();
        curElement.select();
    } 
    /*
     * Undetected Links
     * ----------------
     */
    else if (curElement.hasAttribute( "href" )) {
        /*
         * Handle a possible not detected link.
         */
        return curElement.getAttribute("href");
    }
    /*
     * Last Resort
     * -----------
     */
    else {
        /*
         * This is none of the elements we know (as yet) but we can try to
         * click there anyway.
         */
        emulateClickOn( curElement );
    }
    /*
     * Return a notification value if there was no link detected so that
     * Jumanji may get out of its "follow" mode.
     */
    if (resetNormalNotification != "")
        return resetNormalNotification;
    else
        return;
}
/*
 * Build Hints Database
 * ====================
 */
/*
 * Find clickable elements in the document
 * ---------------------------------------
 * Scans the child nodes of the current ducument for those being clickable.
 *
 * @return  clickableNodes: array of clickable child nodes collected
 *          labelDigits:    number of label digits required by this
 *                          collateral sequence
 */
function findClickableNodes() {
    /*
     * Make sure to always start in a clear state.
     */
    clearLinkInfo();
    clickableNodes = new Array();
    /*
     * Recursively scan the DOM-provided document links array.
     */
    function addClickableNodesIn( thisParent ) {
        for (var i = 0; i < thisParent.childNodes.length; i++) {
            var curNode = thisParent.childNodes[i];
            /*
             * Look at available and visible type 1 nodes only.
             */
            if (curNode.nodeType == 1 &&
                isDisplayable( curNode ) && 
                isVisible( curNode )) {
                /*
                 * Check if this is a clickable element and
                 * add it to the clickableNodes array if so.
                 */
                if (isClickable( curNode )) {
                    clickableNodes.push( curNode );
                }
            }
            /*
             * Recursively check for clickable nodes in the childs of this one.
             */
            addClickableNodesIn( curNode );
        }
    }
    /*
     * Now start this scan at the document root.
     */
    addClickableNodesIn( document );
    /*
     * If wanted now find an optimal collateral sequence for labels display.
     */
    var curLength = clickableNodes.length;
    /*
     * Do so only if there are any clickable nodes at all.
     */
    if (curLength > 0) {
        hasLinkNodes = true;
    } else {
        hasLinkNodes = false;
        return;
    }

    if (collSequence == "optimal") {
        if (useNumbersInOptimal && curLength < 10) {
            // Labels need one number digit only.
            useSequence = "0123456789";
            useBase = 10;
        } 
        else if (curLength < 50) {
            // Labels displayable with one longalpha digit.
            useSequence = "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ";
            useBase = 50;
        } 
        else if (curLength > 99) {
            // Labels would need more than three number digits.
            // Note: This could as well be a lower + upper case sequence but
            //       using lower case only appears to be more practical.
            useSequence = "abcdefghijklmnopqrstuvxyz";
            useBase = 25;
        } 
        else if (useNumbersInOptimal) {
            // Labels displayable with two number digits.
            useSequence = "0123456789";
            useBase = 10;
        }
        else {
            // Use letters anyway.
            useSequence = "abcdefghijklmnopqrstuvxyz";
            useBase = 25;
        }
    }
    /*
     * Finally compute the number of digits the labels need to show.
     */
    labelDigits = 0;
    if (curLength == 1)
        labelDigits = 1;
    else
        while (curLength > 1) {
            labelDigits++;
            curLength /= useBase;
        }
}
/*
 * Create Overlays
 * ---------------
 * Requires the clickableNodes being evaluated already and no overlays being
 * created yet.
 *
 * @return  labelsOverlays:   array of hinting label elements
 *          nodeLabels:       array of hinting label texts
 *          elementsOverlays: array of element node overlays
 */
function createOverlays() {
    var curElement;
    var curLabel;
    var curOverlay;
    var overlayPosition;
    /*
     * Do nothing if there are no clickable nodes at all.
     */
    if (! hasLinkNodes)
        return;
    /*
     * Scan the clickableNodes and construct a labels overlay for each.
     */
    elementsOverlays = new Array();
    labelsOverlays = new Array();
    nodeLabels = new Array();

    for (var i = 0; i < clickableNodes.length; i++) {
        curLabel = labelTextOf( i );
        curElement = clickableNodes[i];
        /*
         * Hinting label overlays
         * ----------------------
         */
        overlayPosition = positionOf( curElement, 6, 6, 1, 1 );

        curOverlay      = document.createElement( "span" );
        curOverlay.id   = overlayId;

        curOverlay.style.position   = "absolute";
        if (! rawMode) {
            curOverlay.style.top    = overlayPosition[0] + "px";
            curOverlay.style.left   = overlayPosition[1] + "px";
        }
        curOverlay.style.width      = "auto";
        curOverlay.style.height     = "auto";
        curOverlay.style.padding    = "1px";
        curOverlay.style.background = nodeLabelBackground[displayMode];
        curOverlay.style.fontSize   = nodeLabelSize;
        curOverlay.style.fontWeight = 'bold';
        curOverlay.style.fontColor  = nodeLabelColor[displayMode];
        curOverlay.style.textTransform = "none";
        //
        curOverlay.style.zorder = 10000;    // always on top
        curOverlay.style.opacity = nodeOpacity;
        //
        curOverlay.style.border     = nodeLabelBorderWidth[displayMode] + "px " +
                                        nodeLabelBorderStyle[displayMode] + " " +
                                        nodeLabelBorder[displayMode];
        //
        curOverlay.style.visibility = "hidden";
        // This will be displayed:
        curOverlay.innerHTML = 
            "<font color=\"" + 
            nodeLabelColor[displayMode] + "\">" + 
            curLabel + 
            "</font>";
        //
        labelsOverlays.push( curOverlay );
        nodeLabels.push( curLabel );
        /*
         * Insert this into the document as sibling of the current element.
         */
        curElement.parentNode.insertBefore( curOverlay, curElement );
        /*
         * Element node overlays
         * ---------------------
         */
        overlayPosition = positionOf( curElement, 0, 0, 0, 0 );

        curOverlay    = document.createElement( "span" );
        curOverlay.id = overlayId + "Element";

        curOverlay.style.position   = "absolute";
        if (! rawMode) {
            curOverlay.style.top    = overlayPosition[0] + "px";
            curOverlay.style.left   = overlayPosition[1] + "px";
        }
        curOverlay.style.width      = overlayPosition[2] + "px";
        curOverlay.style.height     = overlayPosition[3] + "px";
        curOverlay.style.background = navElementBackground[displayMode];
        //
        curOverlay.style.zorder     = 10000;    // always on top
        curOverlay.style.opacity    = navElementOpacity;
        //
        curOverlay.style.border     = navElementBorderWidth[displayMode] + "px " + 
                                        navElementBorderStyle[displayMode] + " " +
                                        navElementBorder[displayMode];
        //
        curOverlay.style.visibility = "hidden";
        //
        elementsOverlays.push( curOverlay );
        /*
         * Insert this into the document as sibling of the current element.
         */
        curElement.parentNode.insertBefore( curOverlay, curElement );
    }
}

/********************************
 *** Hinting Based Navigation ***
 ********************************/
/*
 * Hinting Labels Display
 * ======================
 */
/*
 * Switch labels display
 * ---------------------
 * Changes the label appearances to the colors given by displayMode.
 */
function switchLabelsDisplay() {
    var curLabel;
    /*
     * Do nothing if there are no clickable nodes at all.
     */
    if (! hasLinkNodes)
        return;

    for (var i = 0; i < labelsOverlays.length; i++) {
        curLabel = labelsOverlays[i];

        curLabel.style.background = nodeLabelBackground[displayMode];

        curLabel.style.border = nodeLabelBorderWidth[displayMode] + "px " +
                                nodeLabelBorderStyle[displayMode] + " " +
                                nodeLabelBorder[displayMode];
    }
}
/*
 * Show Labels Overlays
 * --------------------
 * Shows overlays starting with labelHead, hides all others.
 * If no direct match yet show the label tails only. Else show the complete
 * label.
 *
 * @param   labelHead   initial character sequence of the labels to be shown
 *                      where only the remaining tail will be displayed
 *                      if "*":   show all labels without change
 *                      if "":    reset and show all labels
 * @return  curLabelNum number of first detected label in the current viewport
 *                      otherwise the number of the first label of the selected 
 *                      group
 */
function showLabelOverlays( labelHead ) {
    var curLabel;
    var curOverlay;
    var firstLabelNum;
    var portLabelNum;
    var groupLabelFound;
    var headLength = labelHead.length;
    var visibility;

    var displayString;
    var displayBackground;
    var displayColor;
    /*
     * Do nothing if there are no clickable nodes at all or if the labelHead 
     * does not denote a valid labels group.
     */
    if (! hasLinkNodes)
        return;
    if (! isValidLabel( labelHead )) // (sets curlabelNum to element group top)
        return;
    /*
     * The color which will be used for partially selected labels depends on
     * whether they will be shown fully or shortened to their tails.
     * (We need to be able to distinguish selected labels from not selected
     * ones in full labels display.)
     */
    displayBackground = partialLabelBackground[displayMode];
    displayColor = partialLabelColor[displayMode];
    if (shortenLabels) {
        displayBackground = nodeLabelBackground[displayMode];
        displayColor = nodeLabelColor[displayMode];
    }
    /*
     * Scan the labels overlays array.
     */
    portLabelNum = firstLabelNum = -1;
    groupLabelFound = false;

    for (var i = 0; i < labelsOverlays.length; i++) {
        /*
         * Hide all labels initially so that only the interesting ones need
         * be shown.
         */
        labelsOverlays[i].style.visibility = "hidden";
        curLabel = nodeLabels[i];

        displayString = curLabel;
        if (shortenLabels) {
            displayString = curLabel.substring( headLength, labelDigits );
        }
        /*
         * There is no labels group selected
         * ---------------------------------
         */
        if (labelHead == "") {
            /*
             * If there has nothing been selected so far default to the first label
             * on the current page.
             */
            if (curLabelNum < 0) {
                visibility = inViewport( labelsOverlays[i] );
                if ( visibility > -1 && visibility <= 3 ) 
                    curLabelNum = i;
            }
            /* 
             * Mark the last selected label in a "found" color.
             */
            if (i == curLabelNum) {
                labelsOverlays[i].style.background = foundLabelBackground[displayMode];
                labelsOverlays[i].style.fontColor = foundLabelColor[displayMode];
                labelsOverlays[i].innerHTML = 
                    "<font style=\" background: " +
                    foundLabelBackground[displayMode] + "\" color=\"" + 
                    foundLabelColor[displayMode] + "\">" + 
                    displayString +
                    "</font>";
            }
            else {
                /*
                 * Display all other labels normally.
                 */
                labelsOverlays[i].style.background = nodeLabelBackground[displayMode];
                labelsOverlays[i].style.fontColor = nodeLabelColor[displayMode];
                labelsOverlays[i].innerHTML = 
                    "<font style=\" background: " +
                    nodeLabelBackground[displayMode] + "\" color=\"" + 
                    nodeLabelColor[displayMode] + "\">" + 
                    curLabel +
                    "</font>";
            }
            labelsOverlays[i].style.visibility = "visible";
        }
        /*
         * The label belongs to the selected group
         * ---------------------------------------
         */
        else if (curLabel.substring( 0, headLength ) == labelHead) {
            /*
             * Remember the first label of the group
             */
            if (firstLabelNum == -1) {
                firstLabelNum = i;
            }
            visibility = inViewport( labelsOverlays[i] );
            if ( visibility > -1 && visibility <= 3 ) {
                if (! groupLabelFound) {
                    /*
                     * Remember the first label found to be in the viewport.
                     */
                    portLabelNum = i;
                    groupLabelFound = true;
                }
                else if (curLabelNum == i) {
                    /*
                     * Keep the current selection if it already belongs to the
                     * group and lies within the viewport.
                     */
                    portLabelNum = i;   // This will be used for curLabelNum later.
                }
            }
            if (headLength != labelDigits) {
                /*
                 * It is a member of a partially selected hinting labels group.
                 */
                labelsOverlays[i].style.background = displayBackground;
                labelsOverlays[i].style.fontColor = displayColor;
                labelsOverlays[i].innerHTML = 
                    "<font style=\" background: " +
                    displayBackground + "\" color=\"" + 
                    displayColor + "\">" + 
                    displayString +
                    "</font>";
            }
            else {
                /* 
                 * It is a full match, remember this.
                 */
                curLabelNum = matchingLabelNum = i;
            }

            labelsOverlays[i].style.visibility = "visible";
        } 
        /*
         * The label does not belong to the selected goup
         * ----------------------------------------------
         */
        else {
            /* 
             * Restore to full label representation.
             */
            labelsOverlays[i].style.background = nodeLabelBackground[displayMode];
            labelsOverlays[i].style.fontColor = nodeLabelColor[displayMode];
            labelsOverlays[i].innerHTML =
                "<font color=\"" + 
                nodeLabelColor[displayMode] +
                "\" style=\"background: " +
                nodeLabelBackground[displayMode] + "\">" + 
                curLabel + 
                "</font>";
            /*
             * If we do not shorten this label needs to be shown.
             */
            if (! shortenLabels) 
                labelsOverlays[i].style.visibility = "visible";
        }
    }
    /*
     * If we did not select a label already, use the first label of the
     * group if any, then reposition the viewport if necessary.
     * But leave the postion alone if only a reselect of all labels were made.
     */
    if (groupLabelFound) {
        /*
         * There was a group label found to be in the viewport. 
         * Put the current selection there.
         */
        if (portLabelNum == -1)
            curLabelNum = firstLabelNum;
        else
            curLabelNum = portLabelNum;
    }
    else if (firstLabelNum != -1) {
        /*
         * A group was selected with no label of it being visible.
         * Show the first labal of this group then.
         */
        curLabelNum = firstLabelNum;
    }
    /*
     * Finally move the current selection group in the viewport if necessary
     * and mark the current label visibly.
     */
    if (curLabelNum != -1) {
        visibility = inViewport( labelsOverlays[curLabelNum] );
        /*
         * Adjust the viewport in any case if the label is not fully visible.
         */
        if ( visibility != 0 ) {
            adjustViewportFor( labelsOverlays[curLabelNum] );
        }
        /* 
         * Mark the last selected label in a "found" color.
         */
        curLabel = nodeLabels[curLabelNum];
        displayString = curLabel;
        if (shortenLabels) {
            displayString = curLabel.substring( headLength, labelDigits );
        }
        labelsOverlays[curLabelNum].style.background = foundLabelBackground[displayMode];
        labelsOverlays[curLabelNum].style.fontColor = foundLabelColor[displayMode];
        labelsOverlays[curLabelNum].innerHTML = 
            "<font style=\" background: " +
            foundLabelBackground[displayMode] + "\" color=\"" + 
            foundLabelColor[displayMode] + "\">" + 
            displayString +
            "</font>";
    }
}
/*
 * Hide labels overlays
 * --------------------
 * Hides every label overlay.
 */
function hideLabelOverlays() {
    /*
     * Do nothing if there are no clickable nodes at all.
     */
    if (! hasLinkNodes)
        return;
    /*
     * Scan the labels overlays array and hide the nodes displays.
     */
    for (var i = 0; i < labelsOverlays.length; i++) {
        labelsOverlays[i].style.visibility = "hidden";
    }
}
/*
 * Display overlays again
 * ----------------------
 */
// function redisplayOverlays() {
//     showLabelOverlays( curLabelHead );
// }
/*
 * Remove all overlays
 * -------------------
 * Removes all labels overlays.
 *
 * NOTE: This invalidates the overlays and should not be called out of context.
 */
function removeOverlays() {
    /*
     * Do nothing if there are no overlays at all.
     */
    if (! hasLinkNodes) {
        return;
    }
    /*
     * Track the labels overlays array and remove the node elements kept from
     * their parents.
     */
    for (var i = 0; i < labelsOverlays.length; i++) {
        curNode = labelsOverlays[i];
        curNode.parentNode.removeChild(curNode);
    }
    for (var i = 0; i < elementsOverlays.length; i++) {
        curNode = elementsOverlays[i];
        curNode.parentNode.removeChild(curNode);
    }
}
/**************************************
 *** Element Links Based Navigation ***
 **************************************/
/*
 * Element Links Display
 * =====================
 */
/*
 * Switch elements display
 * -----------------------
 * Switches the displayed colors of both element links and hinting labels
 * according to the current displayMode.
 */
function switchElementsDisplay() {
    var curElement;
    /*
     * Do nothing if there are no clickable nodes at all.
     */
    if (! hasLinkNodes)
        return;

    for (var i = 0; i < elementsOverlays.length; i++) {
        curElement = elementsOverlays[i];

        if (inMatchingMode)
            curElement.style.background = matchElementBackground[displayMode];
        else
            curElement.style.background = navElementBackground[displayMode];

        curElement.style.border = 
            navElementBorderWidth[displayMode] + "px " + 
            navElementBorderStyle[displayMode] + " " +
            navElementBorder[displayMode];
    }
    enhanceElementDisplay( curLabelNum );
}
/*
 * Show clickable element overlays
 * -------------------------------
 */
function showElementOverlays() {
    /*
     * Do nothing if there are no clickable nodes at all.
     */
    if (! hasLinkNodes)
        return;
    /*
     * Scan the element links array restricting the display to the
     * word matching elements if in matching mode.
     */
    if (inMatchingMode) {
        for (var i, j = 0; j < matchingOverlays.length; j++) {
            i = matchingOverlays[j];
            elementsOverlays[i].style.background = matchElementBackground[displayMode];
            elementsOverlays[i].style.visibility = "visible";
        }
    } else if (matchingOverlays) {
        for (var i, j = 0; j < matchingOverlays.length; j++) {
            i = matchingOverlays[j];
            elementsOverlays[i].style.background = navElementBackground[displayMode];
            elementsOverlays[i].style.visibility = "visible";
        }
    } else {
        for (var i = 0; i < elementsOverlays.length; i++) { 
            elementsOverlays[i].style.background = navElementBackground[displayMode];
            elementsOverlays[i].style.visibility = "visible";
        }
    }
    /*
     * Make sure the current link is seen in the viewport.
     */
    enhanceElementDisplay( curLabelNum );
    adjustViewportFor( elementsOverlays[curLabelNum] );
}
/*
 * Show element overlays matching a given pattern
 * ----------------------------------------------
 */
function showMatchingOverlays( thisPattern ) {
    var newOverlays;
    var firstElementNum;
    var portElementNum;
    var groupElementFound;
    var visibility;
    /*
     * Do nothing if there are no clickable nodes at all.
     */
    if (! hasLinkNodes)
        return;

    /*
     * An empty pattern is equivalent to "match all".
     */
    curMatchingPattern = thisPattern;
    if (thisPattern == "")
        thisPattern = ".*";
    /*
     * Rebuild the matching elements array.
     */
    hideElementOverlays();
    if (matchingOverlays)
        matchingOverlays = null;    // Make sure the old array is released.
    matchingOverlays = new Array();

    for (var i = 0; i < clickableNodes.length; i++) {
        if (clickableNodes[i].innerText && clickableNodes[i].innerText != "") {
            if ( clickableNodes[i].innerText.match( thisPattern ) ) {
                /*
                 * We need remember the indexes only as the actual display
                 * will occur in the element links.
                 */
                matchingOverlays.push( i );
            }
        }
    }
    /*
     * It is nothing else to do if there is no match now.
     */
    if (matchingOverlays.length < 1) {
        alert("There is no matching entry for '" + curMatchingPattern + "'");
        return;
    }
    /*
     * Otherwise show this group keeping the last selected element enhanced
     * if it still belongs there. If not select the first one in the group.
     * Uses a different background color from plain navigation to signal the
     * mode we are in.
     */
    portElementNum = firstElementNum = -1;
    groupElementFound = false;

   for (var i, j = 0; j < matchingOverlays.length; j++) {
        i = matchingOverlays[j];
        elementsOverlays[i].style.background = matchElementBackground[displayMode];
        elementsOverlays[i].style.visibility = "visible";

        visibility = inViewport( elementsOverlays[i] );
        if (visibility > -1 && visibility <= 3) {
            if (! groupElementFound) {
                /*
                 * Remember the first group element found to be in the viewport.
                 */
                portElementNum = i;
                groupElementFound = true
            }
            else if (curLabelNum == i) {
                /*
                 * Keep the current selection visible if it already belongs to
                 * the group and lies within the viewport.
                 */
                portElementNum = i;
            }
        }
    }
    /*
     * If there was no element selected already, use the first element of the
     * group if any and reposition the viewort accordingly.
     */
    if (groupElementFound) {
        if (portElementNum == -1)
            curLabelNum = firstElementNum;
        else
            curLabelNum = portElementNum;
    }
    else if (firstElementNum != -1) {
        curLabelNum = firstElementNum;
    }
    /*
     * Make sure the current link is fully visible in the viewport.
     */
    if (curLabelNum != -1) {
        visibility = inViewport( elementsOverlays[curLabelNum] );
        if (visibility != 0 ) {
            adjustViewportFor( matchingOverlays[curLabelNum] );
        }
        enhanceElementDisplay( curLabelNum );
    }
}
/*
 * Hide clickable element overlays
 * -------------------------------
 */
function hideElementOverlays() {
    /*
     * Do nothing if there are no clickable nodes at all.
     */
    if (! hasLinkNodes)
        return;
    /*
     * Remove the enhanced display of the selected label so that the proper
     * element will be enhanced on later visibility change.
     */
    normalElementDisplay( curLabelNum );
    /*
     * Scan the labels overlays array.
     */
    for (var i = 0; i < elementsOverlays.length; i++) 
        elementsOverlays[i].style.visibility = "hidden";
}
/*
 * Go to next node
 * ---------------
 */
function goNextNode() {
    normalElementDisplay( curLabelNum );
    /*
     * If in matching mode constrain the navigation to the available nodes.
     */
    if (matchingOverlays) {
        var newIndex = -1;
        for (var i = 0; i < matchingOverlays.length; i++) {
            if (curLabelNum == matchingOverlays[i]) {
                i = (i == matchingOverlays.length - 1) ? 0 : i + 1;
                newIndex = matchingOverlays[i];
                break;
            }
        }
        if (newIndex == -1)
            curLabelNum = matchingOverlays[0];
        else
            curLabelNum = newIndex;
    } else {
        curLabelNum++;
        if (curLabelNum >= elementsOverlays.length)
            curLabelNum = 0;
    }

    enhanceElementDisplay( curLabelNum );
    adjustLabelHeadTo( curLabelNum );
    adjustViewportFor( elementsOverlays[curLabelNum] );
}
/*
 * Go to previous node
 * -------------------
 */
function goPreviousNode() {
    normalElementDisplay( curLabelNum );
    /*
     * If in matching mode constrain the navigation to the available nodes.
     */
    if (matchingOverlays) {
        var newIndex = -1;
        for (var i = 0; i < matchingOverlays.length; i++) {
            if (curLabelNum == matchingOverlays[i]) {
                i = (i == 0) ? matchingOverlays.length -1 : i - 1;
                newIndex = matchingOverlays[i];
                break;
            }
        }
        if (newIndex == -1)
            curLabelNum = matchingOverlays[0];
        else
            curLabelNum = newIndex;
    } else {
        curLabelNum--;
        if (curLabelNum < 0)
            curLabelNum = elementsOverlays.length - 1;
    }
    enhanceElementDisplay( curLabelNum );
    adjustLabelHeadTo( curLabelNum );
    adjustViewportFor( elementsOverlays[curLabelNum] );
}
/*
 * Navigation Commons
 * ==================
 */
/*
 * Enhance the display of an element
 * ---------------------------------
 * @param   elementNum  number of element to change
 */
function enhanceElementDisplay( elementNum ) {
    elementsOverlays[elementNum].style.background = curNavElementBackground[displayMode];

    elementsOverlays[elementNum].style.border = 
        curNavElementBorderWidth[displayMode] + "px " + 
        curNavElementBorderStyle[displayMode] + " " +
        curNavElementBorder[displayMode];
}
/*
 * Set the display of an element back to normal
 * --------------------------------------------
 * @param   elementNum  number of element to change
 */
function normalElementDisplay( elementNum ) {
    if (inMatchingMode) {
        elementsOverlays[elementNum].style.background = matchElementBackground[displayMode];
    }
    else {
        elementsOverlays[elementNum].style.background = navElementBackground[displayMode];
    }

    elementsOverlays[elementNum].style.border = 
        navElementBorderWidth[displayMode] + "px " + 
        navElementBorderStyle[displayMode] + " " +
        navElementBorder[displayMode];
}
/*
 * Keep displayed element in the viewport
 * --------------------------------------
 * Tries to show as much of the element as does fit in the window but always
 * keeps the top left corner visible.
 *
 * @param   thisElement element to adjust the viewport to
 */
function adjustViewportFor( thisElement ) {
    var horShift = 0;
    var vertShift = 0;
    var outside = inViewport( thisElement );

    // Do nothing if the element fits in the viewport.
    if (outside == 0) {
        return;
    }
    /*
     * Calculate the possible shift offsets.
     */
    var elPos = absolutePositionOf( thisElement );
    var winPos = viewportPosition();
    var offset = [0, 0, 0, 0];
    for (var i = 0; i < 4; i++) {
        offset[i] = elPos[i] - winPos[i];
    }
    /*
     * Calculate the offsets the viewport has to be shifted.
     * Positioning the top left corner gets precedence.
     */
    if (outside == -1) {
        // Element is fully outside, get it in.
        if (elPos[0] < winPos[0] || elPos[3] > winPos[3])
            vertShift = offset[0];
        if (elPos[1] < winPos[1] || elPos[2] > winPos[2])
            horShift = offset[1];
    }
    else {
        if (outside / 8 >= 1) {
            // bottom edge outside
            outside -= 8;
            // Do not shift the top edge out.
            if (offset[3] > offset[0])
                vertShift = offset[0];
            else
                vertShift = offset[3];
        }
        if (outside / 4 >= 1) {
            // right edge outside
            outside -= 4;
            // Do not shift the left edge out.
            if (offset[2] > offset[1])
                horShift = offset[1];
            else
                horShift = offset[2];
        }
        if (outside / 2 >= 1) {
            // Left edge outside, shift this one in.
            outside -= 2;
            horShift = offset[1];
        }
        if (outside == 1) {
            // Top edge outside, shift this one in.
            vertShift = offset[0];
        }
    }
    /*
     * And now shift the viewport.
     */
    window.scrollBy( horShift, vertShift );
}
/*
 * Switch raw position mode
 * ------------------------
 * Raw position mode places the element links as well as the hinting labels at the
 * top left corner of the node the overlay was attached to.
 * Otherwise the placement will be relative to top left corner of the computed element
 * rectangle.
 * This is necessary because both positions vary on a wide range of web pages, probably
 * because of a position reporting webkit (?, not really sure) problem.
 */
function switchRawMode() {
    rawMode = ! rawMode;
    /*
     * This needs a databases rebuild in order to keep track of inheritance.
     */
    removeOverlays();
    createOverlays();

    if (navigationMode)
        showElementOverlays();
    else
        showLabelOverlays( curLabelHead );
}
/*
 * Switch display mode
 * -------------------
 * Toggles the displayMode variable and changes the color attributes of
 * both the hinting labels and the element link nodes accordingly.
 */
function switchDisplayMode() {
    if (displayMode == 0)
        displayMode = 1;
    else
        displayMode = 0;

    switchElementsDisplay();
    switchLabelsDisplay();

    if (navigationMode)
        showElementOverlays();
    else
        showLabelOverlays( curLabelHead );
}
/*
 * Switch to hinting mode
 * ----------------------
 */
function useHintingMode() {
    /*
     * The label position might have changed, account for it.
     */
    var labelLength = lastLabelHead.length;

    curLabelHead = nodeLabels[curLabelNum];
    if (labelLength == 0)
        curLabelHead = "";
    else
        curLabelHead = curLabelHead.substr( 0, labelLength );

    navigationMode = false;
    hideElementOverlays();
    showLabelOverlays( curLabelHead );
}
/*
 * Switch to navigation mode
 * -------------------------
 */
function useNavigationMode() {
    navigationMode = true;
    hideLabelOverlays();
    /*
     * Show the previously selected label or go to top.
     */
    showElementOverlays();
}

/******************************
 *** Jumanji Main Interface ***
 ******************************/
/*
 * Start Navigating
 * ================
 */
function show_hints() {
    initializeAll();
    if (hasLinkNodes) {
        showLabelOverlays( "" );
    } else {
        alert( "No clickable node found on this page." );
    }
}
/*
 * Navigate the hinted elements
 * ============================
 * @param   labelHead   head string of the label group to display
 * @return  URI of page to select if no action taken here
 *          else nothing
 */
function update_hints( labelHead ) {
    /*
     * Strip possible command character from the label head string
     * -----------------------------------------------------------
     * @return  labelHead if none of the command characters was appended
     *          else labelHead string without the last character
     *          provided this one is valid in the given mode.
     */
    function stripCommandChar() {
        if (navigationMode) {
            if (labelHead.length > 0) {
                var lastChar = labelHead.charAt(labelHead.length-1);
                if (lastChar == clickNodeChar ||
                    lastChar == modeSwitchChar ||
                    lastChar == matchOffChar) {
                    return labelHead.substr(0, labelHead.length - 1);
                }
                if (! inMatchingMode) {
                    if (lastChar == matchOnChar ||
                        lastChar == displaySwitchChar ||
                        lastChar == switchRawChar ||
                        lastChar == nextNodeChar ||
                        lastChar == previousNodeChar) {
                        return labelHead.substr(0, labelHead.length - 1);
                    }
                }
            }
        }
        return labelHead;
    }
    /*
     * Nothing to do if there are no clickable nodes.
     */
    if (! hasLinkNodes)
        return resetNormalNotification;
    /*
     * Evaluate the label head string if a command was entered.
     */
    if (labelHead.length > 0) {
        curNavCommand = labelHead.charAt(labelHead.length-1);
        /*
         * Check if modes should be switched.
         */
        if (curNavCommand == modeSwitchChar) {
            /*
             * This is a mode dependent toggle. It switches between navigation
             * mode on on hand and word matching or hinting mode on the other.
             */
            if (! navigationMode) {
                useNavigationMode();
                return clearBufferNotification;
            } 
            else if (! inMatchingMode) {
                if (matchingOverlays) {
                    inMatchingMode = true;
                    hideElementOverlays();
                    showMatchingOverlays( curMatchingPattern );
                    return clearBufferNotification + curMatchingPattern;
                }
                else {
                    useHintingMode();
                    return clearBufferNotification + curLabelHead;
                }
            } else {
                inMatchingMode = false;
                hideElementOverlays();
                showElementOverlays();
                return clearBufferNotification;
            }
        }
    }
    else {
        /*
         * There is no label head string.
         */
        curNavCommand = "";
        curLabelHead = "";
    }
    /*
     * Continue navigation in the selected mode.
     */
    if (! navigationMode) {
        /*
         * This is hinting mode.
         */
        if (curNavCommand == clickNodeChar) {
            matchingLabelNum = curLabelNum;
        }
        else {
            lastLabelHead = curLabelHead = stripCommandChar();
            showLabelOverlays( curLabelHead );
        }
    } 
    else if (inMatchingMode) {
        /*
         * This is word matching mode.
         */
        switch (curNavCommand) {
        case clickNodeChar:
            matchingLabelNum = curLabelNum;
            break;
        case matchOffChar:
            inMatchingMode = false;
            matchingOverlays = null;
            lastMatchingPattern = null;
        case modeSwitchChar:
            inMatchingMode = false;
            hideElementOverlays();
            showElementOverlays();
            break
        default:
                curMatchingPattern = stripCommandChar();
                /*
                 * Do not match the same pattern twice in a row
                 * because jumanji calls this when mode keys only are
                 * pressed as well.
                 */
                if (curMatchingPattern != lastMatchingPattern) {
                    lastMatchingPattern = curMatchingPattern;
                    showMatchingOverlays( curMatchingPattern );
                }
                break;
        }
        /*
         * Synchronize the jumanji global buffer unless we want to click on the
         * current element.
         */
        if (curNavCommand != clickNodeChar) {
            if (matchingOverlays)
                return clearBufferNotification + curMatchingPattern;
            else
                return clearBufferNotification;
        }
    } 
    else {
        showElementOverlays();
        /*
         * Evaluate the navigation commands.
         */
        switch (curNavCommand) {
        case matchOnChar:
            inMatchingMode = true;
            lastMatchingPattern = "";
            hideElementOverlays();
            showMatchingOverlays( curMatchingPattern );
            break;
        case matchOffChar:
            inMatchingMode = false;
            matchingOverlays = null;
            lastMatchingPattern = null;
            hideElementOverlays();
            showElementOverlays();
            break;
        case nextNodeChar:
            goNextNode(); 
            break;
        case previousNodeChar:
            goPreviousNode();
            break;
        case displaySwitchChar:
            switchDisplayMode();
            break;
        case switchRawChar:
            switchRawMode();
            break;
        case clickNodeChar:
            matchingLabelNum = curLabelNum;
            break;
        }
    }
    /*
     * Emulate the mouse action if we found a matching label.
     */
    if (matchingLabelNum != -1) {
        var result=clickNode( matchingLabelNum );
        if (result) {
            return result;
        }
    }
    else if (navigationMode) {
        return clearBufferNotification;
    }
}
/*
 * Stop navigating and clear everything
 * ====================================
 */
function clear() {
    if (navigationMode) {
        hideElementOverlays();
    } else {
        hideLabelOverlays();
    }
    clearLinkInfo();
}

/*** End of JumNav.js ***/
