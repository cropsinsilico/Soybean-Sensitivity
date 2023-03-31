clear;clc;close all;
R=8.314472E-3;%Gas constant KJ mole^{-1} K^{-1}
Rd25 = 1.28;
Qps = 800;% umol / m-2 s-1
Tls = 25; % C
Air_O2 =210;
Vcmax25=[110,110*1.2];
Jmax25 =[195,195*1.2];  %
Rate_TPu = 23;

Air_CO2s = 10:100:1000;
line_colors= {'ko-','ro-'};
for j=1:2
for i=1:length(Air_CO2s)
    Radiation_PAR = Qps;     %umol / m-2 s-1
    LeafTemperature   = Tls; %C
    Ci = Air_CO2s(i);

    Rd = Rd25 * exp(18.72 - 46.39 / (R * (LeafTemperature + 273.15)));
    Rds(i) = Rd;

    GrossAssimilation2=FarquharModel(Vcmax25(j), Jmax25(j), LeafTemperature, Ci, Radiation_PAR, Air_O2,Rate_TPu);
    GrossAssim_farquhar(i,j) = GrossAssimilation2.GrossAssimilation;
    NetAssim_farquhar(i,j)   = GrossAssim_farquhar(i,j) - Rd;
    min_ind(i,j) = GrossAssimilation2.min_ind;
    leafAc(i,j) = GrossAssimilation2.LeafAc;
    leafAj(i,j) = GrossAssimilation2.LeafAj;
    photo_resp_Ac(i,j) = GrossAssimilation2.photo_resp_Ac;
    photo_resp_Aj(i,j) = GrossAssimilation2.photo_resp_Aj;
    photo_resp(i,j) = GrossAssimilation2.photo_resp;
    total_assim(i,j) = GrossAssimilation2.total_assim;
end
    plot(Air_CO2s,NetAssim_farquhar(:,j),line_colors{j},'linewidth',2)
    hold on;
    
end

legend('CTL','V20J20','Location','northwest')
xlabel('Ci (ppm)')

% ylabel('An_{leaf} (\mumol m^-^2 s^-^1)')
ylabel('A_{n,leaf} (\mumol m^-^2 s^-^1)')
% ylabel('Caboxylation rate (\mumol m^-^2 s^-^1)')
% ylabel('Photorespiration rate (\mumol m^-^2 s^-^1)')
% title(['Vcmax=',num2str(Vcmax25),', Jmax=',num2str(Jmax25)])
set(gca,'linewidth',2,'fontsize',20)

var2cal = NetAssim_farquhar;
% var2cal = photo_resp;
ctl = var2cal(Air_CO2s==10,1);
sen = var2cal(Air_CO2s==10,2);
diff = (sen - ctl)
ctl = var2cal(Air_CO2s==410,1);
sen = var2cal(Air_CO2s==410,2);
diff = (sen - ctl)
ctl = var2cal(Air_CO2s==610,1);
sen = var2cal(Air_CO2s==610,2);
diff = (sen - ctl)
