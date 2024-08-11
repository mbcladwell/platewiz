<!-- project#add view template of lnserver
          Please add your license header here.
          This file is generated automatically by GNU Artanis. -->
  
<@include header.tpl %>

  <div class="container">
  <h2>Rearray Hit List HL-<%= hlid %> Using  Plate Set PS-<%= psid %></h2>

  <form action="/hitlist/rearraystep2" method="post">
      <div class="form-group">   
	  <label for="name">Plate Set Name:</label>
	  <input type="text"  class="form-control" id="psname" name="psname" required>
      </div>
      <div class="form-group">   
	  <label for="descr">Description:</label>
	  <input type="text"  class="form-control" id="psdescr" name="psdescr" required>
      </div>
      <div class="form-row">
	  <div class="form-group  col-md-6">
	      <label for="type">Plate Type:</label>
	      <select name="type"  class="custom-select" id="typeid" name="typeid"> <%= plate-types %></select> 
	  </div>
	  <div class="form-group col-md-6">
	      <label for="format">Plate Format:</label>
	      <select name="format"  class="custom-select" id="format">
		  <option value="96">96</option>
		  <option value="384">384</option>
		  <option value="1536">1536</option>
	      </select>
	  </div>
      </div>
 
      <div class="form-row">
	  <div class="form-group col-md-6">
	      <input type="submit"  class="btn btn-primary" value="Submit" id="importButton" name="importButton" enabled>
	  </div>
      </div>

 <input type="hidden" id="prjid" name="prjid" value=<%= prjidq %>>
 <input type="hidden" id="sid" name="sid" value=<%= sidq %>>
 <input type="hidden" id="hlid" name="hlid" value=<%= hlidq %>>
 <input type="hidden" id="numhits" name="numhits" value=<%= numhitsq %>>
 <input type="hidden" id="psid" name="psid" value=<%= psidq %>>


 
</form> 

   <button class="btn btn-primary" type="button" id="loadingButton" name="loadingButton" enabled>
  <span class="spinner-border spinner-border-sm" role="status" aria-hidden="true" ></span>
  Loading...
</button>


  
</div>
  
<script>
 document.getElementById("importButton").style.display = "inline";
 document.getElementById("loadingButton").style.display = "none"; 
 
 function myFunction() {
     var x = document.getElementById("importButton");
     x.style.display = "none";
     var y = document.getElementById("loadingButton");
     y.style.display = "inline";
 }

 var str1="Rearray Hit List HL-";
 var str2=str1.concat(<%=  hlid  %>);
 var str3=str2.concat(" Using Plate Set PS-");
 var str4=str3.concat(<%=  psid  %>);
 document.getElementById("psdescr").value= str4  ;

 /* var temp = <%= format %>;
  * var mySelect = document.getElementById('format');

  * for(var i, j = 0; i = mySelect.options[j]; j++) {
  *     if(i.value == temp) {
  *         mySelect.selectedIndex = j;
  *         break;
  *     }
  * } */
 
</script>




<@include footer.tpl %>

