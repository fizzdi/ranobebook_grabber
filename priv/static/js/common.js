
window.onresize = resizeMenu;
window.onload = resizeMenu;
window.onscroll = function() {fixHeader()};

// Get the header
var header = $("header")[0];

// Get the offset position of the navbar
var sticky = header.offsetTop;

// Add the sticky class to the header when you reach its scroll position. Remove "sticky" when you leave the scroll position
function fixHeader() {
  if (window.pageYOffset > sticky) {
    header.classList.add("sticky");
  } else {
    header.classList.remove("sticky");
  }
}

function resizeMenu()
{
  var hMenu = $('ul#horizontal_menu');
  var leftHeader = $('.pull-left');
  var summaryWidth = 0;

  leftHeader.children().each(function() {
      if (!$(this).is(hMenu))
          summaryWidth += $(this).width();
  });

  if(window.innerWidth < 600){
      hMenu.addClass("mobile-menu");
      hMenu.removeClass("horizontal-menu");
  } else {
      hMenu.removeClass("mobile-menu");
      hMenu.addClass("horizontal-menu");
  }
};

function updateReport(tname)
{

var grid = document.getElementById(tname);

grid.onclick = function(e) {
  var elem = e.target;
  if (elem.tagName == 'INPUT') return;
  if (elem.tagName != 'TH')
    elem = elem.parentNode;

  if (elem.tagName != 'TH')
    return true;

  var newSort;
  switch (elem.getAttribute('data-sorted')) {
    case 'asc':
      newSort = 'desc';
      break;
    default:
      newSort = 'asc';
    }
  sortGrid(grid, elem.cellIndex, elem.getAttribute('data-type'), newSort);
  elem.setAttribute('data-sorted',newSort);
};
}

function sortGrid(grid, colNum, type, sort) {
  var tbody = grid.getElementsByTagName('tbody')[0];

  var rowsArray = [].slice.call(tbody.rows).slice(1);
  var compare;

  if (sort == 'asc')
  {
    switch (type) {
      case 'number':
        compare = function(rowA, rowB) {
          return rowA.cells[colNum].children[0].innerText - rowB.cells[colNum].children[0].innerText;
        };
        break;
      case 'string':
        compare = function(rowA, rowB) {
            A = rowA.cells[colNum].children[0].innerText;
            B = rowB.cells[colNum].children[0].innerText
            if (A === B)
                return rowA.cells[0].children[0].innerText - rowB.cells[0].children[0].innerText;
            else return A.localeCompare(B);
        };
        break;
      case 'time':
        compare = function(rowA, rowB) {
          return Date.parse(rowA.cells[colNum].children[0].innerText) - Date.parse(rowB.cells[colNum].children[0].innerText);
        };
        break;
    }
  }
  else
  {
    switch (type) {
      case 'number':
        compare = function(rowA, rowB) {
          return rowB.cells[colNum].children[0].innerText - rowA.cells[colNum].children[0].innerText;
        };
        break;
      case 'string':
        compare = function(rowA, rowB) {
            A = rowB.cells[colNum].children[0].innerText;
            B = rowA.cells[colNum].children[0].innerText
            if (A === B)
                return rowB.cells[0].children[0].innerText - rowA.cells[0].children[0].innerText;
            else return A.localeCompare(B);
        };
        break;
      case 'time':
        compare = function(rowA, rowB) {
          return Date.parse(rowB.cells[colNum].children[0].innerText) - Date.parse(rowA.cells[colNum].children[0].innerText);
        };
        break;
    }
  }

  rowsArray.sort(compare);

  grid.removeChild(tbody);

  for (var i = 0; i < rowsArray.length; i++) {
    tbody.appendChild(rowsArray[i]);
  }

  grid.appendChild(tbody);

}
