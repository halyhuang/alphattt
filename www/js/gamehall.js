var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy("hall.yaws", [ "get_hallState","set_room","get_RankList","get_OneLineList" ]);
var auth_service = new jsonrpc.ServiceProxy("auth.yaws", ["is_login"]);

var hall = 
{
	data:null,	//所有游戏房间数据
	pageSize:4,	//每页显示的游戏房间行数
	lineSize:3,	//行显示的游戏房间行数
	curPage:0,	//当前页码
	pageNum:0,	//总页数
	roomNum:0	//游戏房间数
};

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
	getHallState();
	DisplayHall();
}

function ShowRankList()
{
	CreateRankList(getRankList(),"#ranklist");
}

function ShowOnLineList()
{
	CreateOnLineUsersList(getOneLineList(),"#onlinelist");
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
		hall.data = service.get_hallState();		
		hall.roomNum = hall.data.rooms.length;
		var lineNum=hall.roomNum % hall.lineSize==0 ? hall.roomNum/hall.lineSize : Math.floor(hall.roomNum/hall.lineSize)+1;//根据记录条数，计算行数
		hall.pageNum = lineNum % hall.pageSize==0 ? lineNum/hall.pageSize : Math.floor(lineNum/hall.pageSize)+1;//根据记录条数，计算页数
     } catch(e) 
	 {
        alert(e);
     }	
}

// 根据游戏大厅数据生成大厅的HTML脚本
function DisplayHall()
{ 
	if( !hall.data )
		return;
	var table = $("#gamehalltb");
	table.empty();
	CreateHall(table);
	DisplayPages(table);

}	

// 根据游戏大厅数据生成大厅的HTML脚本
function CreateHall(table)
{ 
	var tdHtmlStart = "<td class=\"tight\">";
	var room;
	var roomIndex = hall.curPage * hall.pageSize * hall.lineSize;
	var tr;
	var td;
	for( var i=0; i < hall.pageSize; i++ )
	{
		tr=$("<tr></tr>");
		tr.appendTo(table);
		for( var j=0; j < hall.lineSize; j++ )
		{
			if(roomIndex < hall.roomNum)
			{
				room = hall.data.rooms[roomIndex];
				td=$("<td class=\"tight\"><table class=\"gamehall\">"	+
						"<tr>" + 
							tdHtmlStart + getPlayerHtml(room.player1,room.playertype1) + getOnClickStr(room.roomId,room.player1)+
							tdHtmlStart + getTableHtml(room.roomId) + getOnClickStr(room.roomId,room.player1)+
							tdHtmlStart + getPlayerHtml(room.player2,room.playertype2) + getOnClickStr(room.roomId,room.player2)+
						"</tr>	"+
						"</table></td>");
			   td.appendTo(tr);
			   roomIndex++;
		   }
		   else	// 为保证游戏大厅大小不变，补齐一些无效的房间
		   {
				td=$("<td class=\"tight\"><table class=\"gamehall\">"	+
						"<tr><td class=\"tight\"><br><img width=\"50px\" height=\"50px\" src=\"./image/empty2.jpg\"></td>" + 
							"<td class=\"tight\"><img width=\"60px\" height=\"60px\" src=\"./image/table2.jpg\"></td>"+
						"<td class=\"tight\"><br><img width=\"50px\" height=\"50px\" src=\"./image/empty2.jpg\"></td></tr>	"+
						"</table></td>");
			   td.appendTo(tr);
		   }
		}
	}
}	

function DisplayPages(table)
{
	var pages="";
	for(var i=1; i <= hall.pageNum; i++)
	{
		pages += "<a href=\"#\" onclick=\"Jumpto("+i+")\"> "+i+" </a>";
	}
	tr=$("<tr></tr>");
	tr.appendTo(table);
	td=$("<td class=\"tight\"> 前往 " +pages + "</td>");
	td.appendTo(tr);

}	

function Jumpto(page)
{
	if( page > 0 && page <= hall.pageNum )
	{
		hall.curPage = page-1;
		DisplayHall();
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

function CreateRankList(list,table)
{ 
	if( !list )
		return;
	var color = new Array("#AFAF61","#6FB7B7","#9999CC");
	var user;
	
	ClearTableButHead(table);

	for( var i=0; i < list.users.length; i++ )
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

function CreateOnLineUsersList(list,table)
{ 
	if( !list )
		return;
	var color = new Array("#AFAF61","#6FB7B7","#9999CC");
	var user;
	
	ClearTableButHead(table);

	for( var i=0; i < list.users.length; i++ )
	{
		user = list.users[i];
		var tr = $("<tr bgcolor=\"" + color[i%3] + "\"></tr>");
		tr.appendTo(table);
		var td=$("<td>"	+ user.id +"</td>" +
					"<td>"	+ user.name +"</td>" +
					"<td>"	+ user.roomid +"</td>");
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
	
function getPlayerHtml(player,playertype)
{
	var playerImg;
	
	if (player=="")
		playerImg = "empty2.jpg";
	else
		playerImg = playertype+".png";
	return "<input type=\"image\" title=\""+player +"\" style=\"width:50px;height:50px;\" src=\"./image/"+playerImg;
}
	
function getTableHtml(talbeid)
{
	return talbeid+"号桌<br><input type=\"image\" style=\"width:60px;height:60px;\" src=\"./image/table2.jpg";
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

