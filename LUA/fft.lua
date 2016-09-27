-- Internal variables
local phasevec_exist = 0;
local phasevec = {};

-- Public function
function fft(log2point, xy_in)
    local i;
    if phasevec_exist == 0 then
	for i=1, 32 do
	    local point = bit.lshift(1, i);
	    phasevec[i] = {math.cos(-2*math.pi/point), math.sin(-2*math.pi/point)};
	end
	phasevec_exist = 1;
    end

    local point=bit.lshift(1, log2point)
    local xy_out = {n=point}

    for i=1, point do
	local brev = i-1;
	brev = bit.bor(bit.rshift(bit.band(brev, 0xaaaaaaaa), 1), bit.lshift(bit.band(brev, 0x55555555), 1));
	brev = bit.bor(bit.rshift(bit.band(brev, 0xcccccccc), 2), bit.lshift(bit.band(brev, 0x33333333), 2));
	brev = bit.bor(bit.rshift(bit.band(brev, 0xf0f0f0f0), 4), bit.lshift(bit.band(brev, 0x0f0f0f0f), 4));
	brev = bit.bor(bit.rshift(bit.band(brev, 0xff00ff00), 8), bit.lshift(bit.band(brev, 0x00ff00ff), 8));
	brev = bit.bor(bit.rshift(brev, 16), bit.lshift(brev, 16));

	brev = bit.rshift(brev, 32-log2point);
	xy_out[brev+1] = { xy_in[i][1], xy_in[i][2] };
    end

    -- here begins the Danielson-Lanczos section
    local n = bit.lshift(1, log2point);
    local l2pt=1;
    local mmax=1;
    while n > mmax do
	local istep = bit.lshift(mmax, 1);

--	double theta = -2*M_PI/istep;
--	double complex wphase_XY = cos(theta) + sin(theta)*I;
	local wphase_XY = phasevec[l2pt];
	l2pt = l2pt+1;

	local w_XY = {1.0, 0.0};
	for m=1, mmax do
	    for i=m, n, istep do
		local tempXY = { w_XY[1] * xy_out[i+mmax][1] - w_XY[2] * xy_out[i+mmax][2], 
				 w_XY[1] * xy_out[i+mmax][2] + w_XY[2] * xy_out[i+mmax][1] };

		xy_out[i+mmax][1]  = xy_out[i][1] - tempXY[1];
		xy_out[i+mmax][2]  = xy_out[i][2] - tempXY[2];

		xy_out[i     ][1] = xy_out[i     ][1] + tempXY[1];
		xy_out[i     ][2] = xy_out[i     ][2] + tempXY[2];
	    end
	    local w_XY_t;
	    w_XY_t  = w_XY[1] * wphase_XY[1] - w_XY[2] * wphase_XY[2]; -- rotate
	    w_XY[2] = w_XY[1] * wphase_XY[2] + w_XY[2] * wphase_XY[1]; -- rotate
	    w_XY[1] = w_XY_t;
	end
	mmax=istep;
    end
    return xy_out;
end
