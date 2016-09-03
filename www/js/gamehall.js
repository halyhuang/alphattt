var serviceURL = "alphattt.yaws";
var methods = [ "get_hallState", "enter_room", "get_username"];

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy(serviceURL, methods);

function Click(tablied,player)
{
	var username = $.cookie("username");
	var password = $.cookie("password");
    try 
	{
		alert();
	} catch(e) 
	{
		alert(e);
	}
}

function getHallState()
{
    try 
	{
		var hall = service.get_hallState();
		return hall;
     } catch(e) 
	 {
        alert(e);
     }	
}

function getOnClickStr(tableid,player)
{
	return "\" onClick=\"Click('"+tableid+"','"+player+"')\"></td>";
}
	
function getPlayerHtml(player,playerImg)
{
	if (player=="")
		playerImg = "empty.jpg";
	return player + "<br><input id=\"btn"+player+"\" type=\"image\" style=\"width:40px;height:40px;\" src=\"./img/qq.jpg";
}

function updateHall()
{
	var hall = getHallState();
	CreateTable(hall);
}
function CreateTable(hall)
{ 
	if( !hall )
		return;
	var player="player";
	var playerImg="qq.jpg";
	var player1Win = 2;
	var player2Win = 3;
	var tdHtmlStart = "<td class=\"tight\">";
	var roomIndex = 0;
	var room;
	for( var i=0; i<3; i++ )
	{
		var tr=$("<tr></tr>");
		tr.appendTo("#gamehalltb");
		for( var j=0; j<3 && roomIndex < hall.rooms.length; j++ )
		{
			room = hall.rooms[roomIndex];
			var td=$("<td class=\"tight1\"><table class=\"gamehalltable\" id=\"table"+i*3+j+"\">"	+
					"<tr>" + 
						tdHtmlStart + getPlayerHtml(room.player1.name,playerImg) + getOnClickStr(room.roomId,room.player1.name)+
						tdHtmlStart + room.player1.win +":" + room.player2.win+"<br><img src=\"./img/table.jpg\" width=\"60px\" height=\"60px\"></td>"+
						tdHtmlStart + getPlayerHtml(room.player2.name,playerImg) + getOnClickStr(room.roomId,room.player2.name)+
					"</tr>	"+
					"</table></td>");
		   td.appendTo(tr);
		   roomIndex++;
		}
	}
}	

function createHall()
{
    var state = getHallState();
}