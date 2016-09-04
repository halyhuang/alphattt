var desks;

var jsonrpc = imprt("jsonrpc");

var service = new jsonrpc.ServiceProxy("hall.yaws", ["enter_room"]);

window.onload = function() {  	
	init_hall();
};  

function init_hall()
{
	desks = document.querySelectorAll('.desk');
	for (var i=0; i < desks.length; i++)
	{ 
		desks[i].ID = i + 1;	
		desks[i].onclick = enter_room;
	}	
}

function enter_room()
{
    try {
			service.enter_room(this.ID);
			location.href = "alphattt.html";
     } catch(e) {
        alert(e);
     }	
}
