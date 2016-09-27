var desks;

var jsonrpc = imprt("jsonrpc");

var service = new jsonrpc.ServiceProxy("hall.yaws", ["set_room"]);
var auth_service = new jsonrpc.ServiceProxy("auth.yaws", ["is_login"]);

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

window.onload = function() { 
 	check_login();
	init_hall();
};  

function init_hall()
{
	desks = document.querySelectorAll('.desk');
	for (var i=0; i < desks.length; i++)
	{ 
		desks[i].ID = i + 1;	
		desks[i].onclick = set_room;
	}	
}

function set_room()
{
    try {
			service.set_room(this.ID);
			location.href = "alphattt.html";
     } catch(e) {
        alert(e);
     }	
}
