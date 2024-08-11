<!-- plate#getid view template of lnserver
     Please add your license header here.
     This file is generated automatically by GNU Artanis. -->

<@include header.tpl %>
<div class="container">
    <form  id="getwellps">
	<div class="row">
	    <div class="col">
 		
		<h2>Plates for PS-<%= psid %></h2>
	    </div>  
	    <div class="col">
		<div class="dropdown dropright">
		    <button type="button" class="btn btn-primary dropdown-toggle" data-toggle="dropdown">
			<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-tools" viewBox="0 0 16 16">
			    <path d="M1 0L0 1l2.2 3.081a1 1 0 0 0 .815.419h.07a1 1 0 0 1 .708.293l2.675 2.675-2.617 2.654A3.003 3.003 0 0 0 0 13a3 3 0 1 0 5.878-.851l2.654-2.617.968.968-.305.914a1 1 0 0 0 .242 1.023l3.356 3.356a1 1 0 0 0 1.414 0l1.586-1.586a1 1 0 0 0 0-1.414l-3.356-3.356a1 1 0 0 0-1.023-.242L10.5 9.5l-.96-.96 2.68-2.643A3.005 3.005 0 0 0 16 3c0-.269-.035-.53-.102-.777l-2.14 2.141L12 4l-.364-1.757L13.777.102a3 3 0 0 0-3.675 3.68L7.462 6.46 4.793 3.793a1 1 0 0 1-.293-.707v-.071a1 1 0 0 0-.419-.814L1 0zm9.646 10.646a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708l-3-3a.5.5 0 0 1 0-.708zM3 11l.471.242.529.026.287.445.445.287.026.529L5 13l-.242.471-.026.529-.445.287-.287.445-.529.026L3 15l-.471-.242L2 14.732l-.287-.445L1.268 14l-.026-.529L1 13l.242-.471.026-.529.445-.287.287-.445.529-.026L3 11z"/>
			</svg>
		    </button>
		    <div class="dropdown-menu">
			<a class="dropdown-item" href="#" onclick="groupPlates()" >Group Plates into Plate Set</a>
			<a class="dropdown-item" href="#" onclick="getWellsForPS('false')" >All Wells for PS-<%= psid %></a>
			<a class="dropdown-item" href="#" onclick="getWellsForPS('true')">Include Controls</a>
		  </div>
		</div>
	    </div>
	</div>  
	

	<table class="display table table-striped table-bordered"><thead><tr><th><img src="../img/checkmark.png" height="20" width="20"></th><th>Plate</th><th>Plate Order</th><th>Plate Type</th><th>Format</th><th>Barcode</th></tr></thead>
    <tbody> <%= body %></tbody>
</table>

    </div>
    <input type="hidden" id="psid" name="psid" value=<%= psidq %>>
</form>

<hr>
<h2>Assay Runs for PS-<%= psid %></h2>
<table class="display table table-striped table-bordered"><thead><tr><th>Assay Run</th><th>Name</th><th>Description</th><th>Type</th><th>Layout</th><th>Layout Name</th></tr></thead>
<tbody> <%= body2 %></tbody>
</table>
<hr>
<h2>Hit Lists for PS-<%= psid %></h2>
<table class="display table table-striped table-bordered"><thead><tr><th>Assay Run</th><th>Hit List</th><th>Name</th><th>Description</th><th>N</th></tr></thead>
 <tbody><%= body3 %></tbody>
  </table>

  <script>

function getWellsForPS(x){
 	var hidden = document.createElement("input");
	hidden.type = "hidden";
	hidden.name = "includecontrols";
	hidden.value = x;
	var f = document.getElementById("getwellps");
       f.appendChild(hidden);
	f.setAttribute("action", "/plate/getwellsforps");
	f.setAttribute("method", "POST");
	f.submit(); return false;
    
   }



   
   $(document).ready(function() {
       $('table.display').DataTable({
        dom: 'lBfrtip',
        buttons: [
            'copyHtml5',
            'excelHtml5',
            'csvHtml5',
            'pdfHtml5'
        ]
    });
 } );

</script>


<@include footer.tpl %>
