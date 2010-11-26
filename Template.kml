<?xml version="1.0" encoding="utf-8" ?>
<kml xmlns="http://www.opengis.net/kml/2.2">
<Document><name>template.kml</name>

<Folder><name>locations</name>

<% for(i in 1:nrow(locations)){ %>
      <Placemark>
        <name><%=paste(locations$location[i])%></name>
        <Point>
          <altitudeMode>relativeToGround</altitudeMode>
          <coordinates><%=locations[i,]$Longitude%>, <%=locations[i,]$Latitude%>, 0</coordinates>
        </Point>
      </Placemark>
<% } %>
      
    </Folder>

<Folder><name>discrete_rates</name>

<% for(i in 1:nrow(out)){ %>
  <Placemark>
          <name><%=paste(i)%></name>
  <Style><LineStyle><color>FFff00ff</color><width><%=as.numeric(levels(out$BF))[i]%></width></LineStyle></Style>
      <LineString><coordinates><%=out[i,]$x%>, <%=out[i,]$y%> 
                               <%=out[i,]$xend%>, <%=out[i,]$yend%>
      </coordinates></LineString>
  </Placemark>
<% } %>  

     </Folder> 

</Document></kml>
