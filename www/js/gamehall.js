var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy("hall.yaws", [ "get_hallState","set_room","get_RankList","get_OneLineList" ]);
var auth_service = new jsonrpc.ServiceProxy("auth.yaws", ["is_login"]);

$(document).ready(function() {
	check_login();
	UpdatePage();
	timerID = setInterval(UpdatePage, 3000);	
	}
)

function is_login()
{
	var r = auth_service.is_login();
	return r.value;
}

function check_login()
{
	if (!is_login())
	{		
		location.href = "login.html";
	}
}

function UpdatePage()
{
	ShowHall();
	ShowRankList();
	SetListHover("#ranklist");
	ShowOnLineList();
	SetListHover("#onlinelist");
}

function ShowHall()
{
	CreateHall(getHallState());
}

function ShowRankList()
{
	CreateList(getRankList(),"#ranklist");
}

function ShowOnLineList()
{
	CreateList(getOneLineList(),"#onlinelist");
}

function enterRoom(tableid)
{
    try {
			service.set_room(tableid);
			location.href = "alphattt.html";
     } catch(e) {
        alert(e);
     }	
}

function getHallState()
{
    try 
	{
		return service.get_hallState();
     } catch(e) 
	 {
        alert(e);
     }	
}

// 根据游戏大厅数据生成大厅的HTML脚本
function CreateHall(hall)
{ 
	if( !hall )
		return;
	var tdHtmlStart = "<td class=\"tight\">";
	var roomIndex = 0;
	var room;
	var TableHtml = "<input type=\"image\" style=\"width:60px;height:60px;\" src=\"./img/table.jpg";
	
	$("#gamehalltb").empty();
	for( var i=0; i<4; i++ )
	{
		var tr=$("<tr></tr>");
		tr.appendTo("#gamehalltb");
		for( var j=0; j<3 && roomIndex < hall.rooms.length; j++ )
		{
			room = hall.rooms[roomIndex];
			var td=$("<td class=\"tight\"><table class=\"gamehall\" id=\"table"+i*3+j+"\">"	+
					"<tr>" + 
						tdHtmlStart + getPlayerHtml(room.player1,"") + getOnClickStr(room.roomId,room.player1)+
						tdHtmlStart + TableHtml + getOnClickStr(room.roomId,room.player1)+
						tdHtmlStart + getPlayerHtml(room.player2,"") + getOnClickStr(room.roomId,room.player2)+
					"</tr>	"+
					"</table></td>");
		   td.appendTo(tr);
		   roomIndex++;
		}
	}
}	

// 查询排名数据
function getRankList()
{
    try 
	{
		return service.get_RankList();
     } catch(e) 
	 {
        alert(e);
     }	
}

// 查询在线用户数据
function getOneLineList()
{
    try 
	{
		return service.get_OneLineList();
     } catch(e) 
	 {
        alert(e);
     }	
}

function CreateList(list,table)
{ 
	if( !list )
		return;
	var color = new Array("#AFAF61","#6FB7B7","#9999CC");
	var user;
	
	ClearTableButHead(table);
	
	//var tr = $("<tr><th>排名</th><th>积分</th><th>昵称</th><th>类型</th></tr>");
	//tr.appendTo(table);
	for( var i=0; i < 10 && i < list.users.length; i++ )
	{
		user = list.users[i];
		var tr = $("<tr bgcolor=\"" + color[i%3] + "\"></tr>");
		tr.appendTo(table);
		var td=$("<td>"	+ (i+1).toString()  +"</td>" +
					"<td>"	+ user.point +"</td>" +
					"<td>"	+ user.name +"</td>" +
					"<td>"	+ user.type +"</td>");
		td.appendTo(tr);
	}
}

function SetListHover(table)
{
	$(table).delegate("tr","mouseover",function(){
		$(this).addClass("over")
	});	
	$(table).delegate("tr","mouseout",function(){
		$(this).removeClass("over");
	});	
}

// 生成点击游戏桌子和椅子的响应调用HTML
function getOnClickStr(tableid)
{
	return "\" onClick=\"enterRoom("+tableid+")\"></td>";
}
	
function getPlayerHtml(player,playerImg)
{
	if (player=="")
		playerImg = "empty.jpg";
	else
		playerImg = "qq.jpg";
	return player + "<br><input type=\"image\" style=\"width:50px;height:50px;\" src=\"./img/"+playerImg;
}

function ClearTableButHead(table)
{
	var cit= $(table);
	if(cit.size()>0) {
	   cit.find("tr:not(:first)").remove();
	}
}

function reserved(tablied,player)
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


