function [lat,lon,dist,bear]=greatcircle(lat1,lon1,lat2,lon2,n)
%GREATCIRCLE "As the crow flies" path, distance and bearing.
%
%	[LAT,LON]=GREATCIRCLE(LAT1,LON1,LAT2,LON2) computes the shortest
%	path along the great circle between two points defined by spherical 
%	coordinates latitude and longitude (in decimal degrees). LAT and LON 
%	contain vectors of coordinates of the way points.
%
%	[LAT,LON,DISTANCE,BEARING]=GREATCIRCLE(...) returns also vectors of
%	distances (in km) using Haversine's formula, and bearing (in degrees
%	from North) along the path way.
%
%	[...]=GREATCIRCLE(...,N) uses N intermediate points (default is 100).
%
%	Example:
%
%	  load topo
%	  contour(0:359,-90:89,topo,[0,0],'k')
%	  [lat,lon,dis] = greatcircle(48.8,2.3,35.7,139.7);
%	  hold on, plot(lon,lat,'r','linewidth',2), hold off
%	  title(sprintf('Paris to Tokyo = %g km',dis(end)))
%
%
%       Author: Francois Beauducel <beauducel@ipgp.fr>
%	Created: 2012-07-26
%	Updated: 2012-07-27
%
%	References:
%	  http://www.movable-type.co.uk/scripts/latlong.html

%	Copyright (c) 2012, François Beauducel, covered by BSD License.
%	All rights reserved.
%
%	Redistribution and use in source and binary forms, with or without 
%	modification, are permitted provided that the following conditions are 
%	met:
%
%	   * Redistributions of source code must retain the above copyright 
%	     notice, this list of conditions and the following disclaimer.
%	   * Redistributions in binary form must reproduce the above copyright 
%	     notice, this list of conditions and the following disclaimer in 
%	     the documentation and/or other materials provided with the distribution
%	                           
%	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
%	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%	ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
%	LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%	SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
%	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
%	CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
%	ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
%	POSSIBILITY OF SUCH DAMAGE.

if nargin < 4
	error('Number of input arguments not correct.')
end

if nargin < 5
	n = 100;
end

% checks that segment is the shortest
if abs(lon2-lon1) > 180
	lon2 = lon2 + 360*sign(lon1-lon2);
end

% defines a linear vector of longitudes
lon = linspace(lon1,lon2,n);

% computes latitudes along the great circle
if lon1 == lon2
	lat = linspace(lat1,lat2,n);
else
	lat = atand((sind(lat1)*cosd(lat2)*sind(lon - lon2) - sind(lat2)*cosd(lat1)*sind(lon - lon1))./(cosd(lat1)*cosd(lat2).*sind(lon1 - lon2)));
end

if nargout > 2
	% computes the distance using Haversine's formula
	a = sind((lat - lat1)/2).^2 + cosd(lat1).*cosd(lat).*sind((lon - lon1)/2).^2;
	dist = 6371*2*atan2(sqrt(a),sqrt(1 - a));
end

if nargout > 3
	% computes the bearing angle
	bear = atan2(sind(lon2-lon).*cosd(lat2),cosd(lat).*sind(lat2) - sind(lat).*cosd(lat2).*cosd(lon2-lon))*180/pi;
	bear(end) = NaN;
end

