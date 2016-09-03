var serviceURL = "alphattt.yaws";
var methods = ["get_state", "start_game", "end_game", "get_move", "get_legal_move", "set_move", "is_login"];

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy(serviceURL, methods);
var grids;
var timerID = 0;

window.onload = function() {  
	check_login();
	init_botton();	
};  

function check_login()
{
	try
	{
		var state = service.is_login();
		if (!state.is_login)
		{
			location.href = "login.html";
		}
	}
	catch(e) 
	{
    	alert(e);
    }

}

function poll()
{
    try {
			var state = service.get_state();
			if (state.is_playing)
			{
				if (state.is_update_move)
				{
					set_legal_move(state.legal_moves);			
					update_move(state.move);
				}	
			}
     } catch(e) {
        alert(e);
     }	
	
}

function init_botton()
{
    var bn_start_game = document.getElementById('play');  
	bn_start_game.onclick = start_game; 
    var bn_end_game = document.getElementById('stop');  
	bn_end_game.onclick = end_game; 	
}  

function init_board()
{
	grids = document.querySelectorAll('.grid');
	for (var i=0; i < grids.length; i++)
	{ 
		grids[i].R = Math.floor((Math.floor(i / 9)) / 3);
		grids[i].C = Math.floor((i % 9) / 3);
		grids[i].r = (Math.floor(i / 9)) % 3;
		grids[i].c = i % 3;		
		grids[i].onclick = click_move;
		grids[i].style.background = 'white';	
		grids[i].innerHTML = "";
		grids[i].state = 0;
		grids[i].is_legal = false;		
	}
}

function grid_pos(move)
{
	return ((move.R * 3 + move.r) * 9 + (move.C * 3 + move.c));
}

function update_move(move)
{
	if (move != "")
	{
		var index = grid_pos(move);
		info("move(" + move.R + "," + move.C + "," + move.r + "," + move.c + ")");
		grids[index].state = 2;
		grids[index].innerHTML = "O";
		grids[index].style.background = '#53FF53';
	}
}

function set_legal_move(legal_moves)
{
	for (var i=0; i < grids.length; i++)
	{ 
		grids[i].style.background = 'white';	
		grids[i].is_legal = false;	
	}	
	for (var i=0; i<legal_moves.length; i++)
	{ 
		var index = grid_pos(legal_moves[i]);
		grids[index].style.background = '#00FFFF';
		grids[index].is_legal = true;
	}	
}

function info(msg)
{
	document.getElementById('result').innerHTML +=
       "<li>" + msg + "</li>";
}

function start_game()
{
	init_board();
	var result = service.start_game();
	info("game start!!!");	
	timerID = setInterval(poll, 1000);	
}  

function end_game()
{
	if (timeID != 0)
	{
		clearInterval(timeID);
		timerID = 0;
	}
	service.end_game();	
	info("end start!!!");	
}

function click_move()
{
	if (this.is_legal)
	{
		service.set_move(this.R, this.C, this.r, this.c);	
		this.state = 1;
		this.innerHTML = "X";	
		info("move(" + this.R + "," + this.C + "," + this.r + "," + this.c + ")");
	}
}