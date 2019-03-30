var h = 36;
var b = 4;
var w = 10
var p = 2;
var startx = p;
var starty = p;
var ctx;

function draw0segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x + p, y);
    ctx.lineTo(x + p + h, y);
    ctx.lineTo(x + p + h - w, y + w);
    ctx.lineTo(x + p + w, y + w);
    ctx.lineTo(x + p, y);
    if (is_stroke)
        ctx.stroke();
    else
        ctx.fill();
}

function draw1segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x + 2 * p + h, y + p);
    ctx.lineTo(x + 2 * p + h, y + h + p - b);
    ctx.lineTo(x + 2 * p + h - b, y + h + p - b);
    ctx.lineTo(x + 2 * p + h - w, y + h + p - w);
    ctx.lineTo(x + 2 * p + h - w, y + p + w);
    ctx.lineTo(x + 2 * p + h, y + p);
    if (is_stroke)
        ctx.stroke();
    else
        ctx.fill();
}

function draw2segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x + 2 * p + h, y + h + 3*p - b);
    ctx.lineTo(x + 2 * p + h, y + 2*h + 3*p - 2*b);
    ctx.lineTo(x + 2 * p + h - w, y + 2*h + 3*p - 2*b - w);
    ctx.lineTo(x + 2 * p + h - w, y + h + 3*p - 2*b + w);
    ctx.lineTo(x + 2 * p + h - b, y + h + 3*p - b);
    ctx.lineTo(x + 2 * p + h, y + h + 3*p - b);
    if (is_stroke)
        ctx.stroke();
    else
        ctx.fill();
}

function draw3segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x + p, y + 2*h + 4*p - 2*b);
    ctx.lineTo(x + p + h, y + 2*h + 4*p - 2*b);
    ctx.lineTo(x + p + h - w, y + 2*h + 4*p - 2*b - w);
    ctx.lineTo(x + p + w, y + 2*h + 4*p - 2*b - w);
    ctx.lineTo(x + p, y + 2*h + 4*p - 2*b);
    if (is_stroke)
        ctx.stroke();
    else
        ctx.fill();
}


function draw4segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x, y + h + 3*p - b);
    ctx.lineTo(x, y + 2*h + 3*p - 2*b);
    ctx.lineTo(x + w, y + 2*h + 3*p - 2*b - w);
    ctx.lineTo(x + w, y + h + 3*p - 2*b + w);
    ctx.lineTo(x + b, y + h + 3*p - b);
    ctx.lineTo(x, y + h + 3*p - b);
    if (is_stroke)
        ctx.stroke();
    else
        ctx.fill();
}


function draw5segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x, y + p);
    ctx.lineTo(x + w, y + p + w);
    ctx.lineTo(x + w, y + h + p - w);
    ctx.lineTo(x + b, y + h + p - b);
    ctx.lineTo(x, y + h + p - b);
    ctx.lineTo(x, y + p);
    if (is_stroke)
        ctx.stroke();
    else
        ctx.fill();
}

function draw6segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x + p*3.5, y + h);
    ctx.lineTo(x + p*3.5 + w/2, y + h - w/2);
    ctx.lineTo(x + p*3.5 + h - 1.5*w, y + h - w/2);
    ctx.lineTo(x + p*3.5 + h - w, y + h);
    ctx.lineTo(x + p*3.5 + h - 1.5*w, y + h + w/2);
    ctx.lineTo(x + p*3.5 + w/2, y + h + w/2);
    ctx.lineTo(x + p*3.5, y + h);
    if (is_stroke)
        ctx.stroke();
    else
        ctx.fill();
}

function draw7segment(x, y, is_stroke){
    ctx.beginPath();
    ctx.moveTo(x + 4 * p + h, y + 2*h + 3*p - b);
    ctx.lineTo(x + 4 * p + h, y + 2*h + 3*p - b - w);
    ctx.lineTo(x + 4 * p + h + w, y + 2*h + 3*p - b - w);
    ctx.lineTo(x + 4 * p + h + w, y + 2*h + 3*p - b);
    ctx.lineTo(x + 4 * p + h, y + 2*h + 3*p - b);
    if (is_stroke)
        ctx.stroke();
    else
    {
        ctx.fill();
    }
}

function drawText(text, x,y, color)
{
    var oldColor = ctx.fillStyle;
    ctx.fillStyle = color;
    ctx.fillText(text,x,y);
    ctx.fillStyle = oldColor
}

function drawSomeStatus(x,y, cw, ch, isOff)
{
    if (isOff)
        ctx.strokeRect(x,y,cw,ch)
    else
        ctx.fillRect(x,y,cw,ch)
}

function drawStatus(status)
{
    drawSomeStatus(10,82,15,8, (status&(1<<7)) != 0);
    drawText("СТАБ", 5,100, "#000");

    drawSomeStatus(60,82,15,8, (status&(1<<1)) != 0);
    drawText("НОЛЬ", 55,100, "#000");

    drawSomeStatus(110,82,15,8, (status&(1<<2)) != 0);
    drawText("НЕТТО", 101,100, "#000");

    drawSomeStatus(160,82,15,8, (status&(1<<3)) != 0);
    drawText("БРУТТО", 150,100, "#000");

    drawSomeStatus(210,82,15,8, (status&(1<<4)) != 0);
    drawText("1", 227,100, "#000");

    drawSomeStatus(240,82,15,8, (status&(1<<5)) != 0);
    drawText("2", 257,100, "#000");

    drawSomeStatus(270,82,15,8, (status&(1<<6)) != 0);
    drawText("3", 287,100, "#000");

    drawSomeStatus(320,82,15,8, (status&(1<<0)) != 0);
    drawText("ГОТОВ", 311,100, "#000");
}

function initScoreboard(divname)
{
                            //    <canvas id = "mainScoreboard" style="border:solid black 1px;margin-bottom:2px; background-color:black" height=105 width=348></canvas>
    var canvas = document.createElement('canvas');

    canvas.id = "cnvscore";
    canvas.width = 348;
    canvas.height = 105;
    canvas.style = "1px solid black;";


    var body_div = $('#' + divname).get(0);
    body_div.appendChild(canvas);

    //var canvas = $('#cnvscore').get(0);
    if (!canvas.getContext)
        return false;
    ctx = canvas.getContext('2d');
    clearScoreboard();
    ctx.fillStyle = '#000'
    ctx.strokeStyle = '#bbb'
    addSymbol(0xff);
    addSymbol(0xff);
    addSymbol(0xff);
    addSymbol(0x40);
    addSymbol(0xc0);
    addSymbol(0xc0);
    drawStatus(0x2f);
    return true;
}

function addSymbol(x)
{
    draw7segment(startx, starty, (x&(1<<7)) != 0);
    draw6segment(startx, starty, (x&(1<<6)) != 0);
    draw5segment(startx, starty, (x&(1<<5)) != 0);
    draw4segment(startx, starty, (x&(1<<4)) != 0);
    draw3segment(startx, starty, (x&(1<<3)) != 0);
    draw2segment(startx, starty, (x&(1<<2)) != 0);
    draw1segment(startx, starty, (x&(1<<1)) != 0);
    draw0segment(startx, starty, (x&(1<<0)) != 0);
    startx += 6 * p + h + w;
}

function drawScoreboard(ledStatus)
{
    startx = p;
    clearScoreboard();
    if (ledStatus == undefined || ledStatus.length < 7)
        ledStatus = [0x00,0xbf, 0xbf,0xbf,0xbf,0xbf,0xbf];
    drawStatus(ledStatus[0]);
    addSymbol(ledStatus[1]);
    addSymbol(ledStatus[2]);
    addSymbol(ledStatus[3]);
    addSymbol(ledStatus[4]);
    addSymbol(ledStatus[5]);
    addSymbol(ledStatus[6]);
    addSymbol(ledStatus[7]);
}

function clearScoreboard()
{
    ctx.clearRect(0,0,6000,6000);
}

var ledtest = [0, 0, 0, 0, 0, 0, 0];
var ledind = [0, 0, 1, 2, 3, 4, 5];
var nums = [192, 249, 164, 176, 153, 146, 130, 248, 128, 144]

var dig = 0;

function modifyLed(pos)
{
    ledtest[pos] = nums[ledind[pos]];
    ledind[pos]++;
    ledind[pos]%=10;
    drawScoreboard(ledtest);
}

function startTestScoreboard()
{
  setInterval(function(){
    var t = "00000" + dig.toString();
    for (var i = 0; i < 6; ++i)
    {
        ledtest[6-i] = nums[t[t.length - 1 - i] - '0'];
    }
    dig++;
    drawScoreboard(ledtest);
  }, 500);

}