rng(8902809)
wks_per_yr = 52;
nyrs = 50;
npops = 2;
nfleets = 3;
ships_per_fleet = [1, 1, 1];
fleet_permits = [1, 0;
                 0, 1;
                 1, 1]; % final column = permit to not fish
pop_seasons = ones(2, wks_per_yr);
% crab (spp. 1) season = Dec. 1 - Aug. 14
% Assume yr starts Dec. 1
pop_seasons(1, 37) = 0.7;
pop_seasons(1, 38:wks_per_yr) = 0;
            
% salmon (spp. 2) season = May 1 - Oct. 31 (actually more complicated)
pop_seasons(2, 1:21) = 0;
pop_seasons(2, 18) = 0.3;
pop_seasons(2, 47) = 0.7;
pop_seasons(2, 48:wks_per_yr) = 0;

catchability = [.05, .01]; 
% proportion of stock that will be caught by one fleet/ship during one week
% of fishing
cost_price_ratio = [.001, .001];

Catch = zeros(npops, nyrs, wks_per_yr, nfleets);
N = zeros(npops, nyrs, wks_per_yr);
N(:,:,1) = exp(randn(npops, nyrs)); % assume average Recruitment = 1
N(2,:,1) = 0.1 * N(2,:,1);
len_at_rec = exp(randn(npops, nyrs)); % will need average sizes here
% also need len -> weight parameters (or could simulate weight @
% recruitment?
% Right now: white noise, independence between stocks. 
% To do: correlation in time and between stocks.

for yr = 1:nyrs
    
    for wk = 1:wks_per_yr
        exp_util = [exp(N(:,yr,wk)' .* catchability - cost_price_ratio).*...
            pop_seasons(:,wk)', 1];
        probs = [exp_util, 1] / sum([exp_util, 1])
        
        % simulate whether/which pop'n to fish = nominal effort
        % for now: use probabilities to distribute effort
        % convert nominal effort --> C
        % second term sums catch for each spp over the fleets
        if wk < wks_per_yr
            N(:,yr,wk+1) = N(:,yr,wk) - sum(Catch(:,yr,wk,:), 4);
        end
    end
end