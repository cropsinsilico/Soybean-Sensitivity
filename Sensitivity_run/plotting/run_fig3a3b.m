clear;clc;close all;
R=8.314472E-3;%Gas constant KJ mole^{-1} K^{-1}
Rd25 = 1.28;
Qps = 1500;% umol / m-2 s-1
Tls = 25; % C
Air_O2 =210;
Vcmax25=[110,110];
Jmax25 =[195,195*1.2];  %
Rate_TPu = 23; %set a large value to turn Ap off
Air_CO2s = 50:100:1000;
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
end
    if(j==1)
        plot(Air_CO2s,leafAc(:,j),'-','color',[0.8500 0.3250 0.0980],'linewidth',2)
        hold on
        plot(Air_CO2s,leafAj(:,j),'-','color',[0 0.4470 0.7410],'linewidth',2)
        hold on;
    else
        plot(Air_CO2s,leafAj(:,j),'--','color',[0 0.4470 0.7410],'linewidth',2)
        hold on;
    end
end
grid on
xlabel('Ci (ppm)')
ylabel('Assimilation rate (\mumol m^-^2 s^-^1)')
legend('Ac(CTL)','Aj(CTL)','Aj(Jmax+20%)')
% title(['Vcmax=',num2str(Vcmax25),', Jmax=',num2str(Jmax25)])
set(gca,'linewidth',2,'fontsize',20)


