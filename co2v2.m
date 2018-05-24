function co2v2
% CO2  GUI application to process, store and plot soil CO2 measurements.

% Author: Douglas M. Schwarz
% Date:   9 May 2006
% Modified 1 May 2007, added fixed y-axis scaling to flux graphs.
% Modified 12 September 2007, new gas exchange calculations.
% Version 2.0
% E-mail: dmschwarz (at) ieee (dot) org
% Written under contract to the United States Geological Survey.

ss = get(0,'ScreenSize');
wh = [280 340];
gray = get(0,'DefaultUIControlBackgroundColor');
fig = figure('Position',[0 0 wh],...
	'NumberTitle','off',...
	'Name',mfilename,...
	'Menubar','none',...
	'IntegerHandle','off',...
	'Color',gray,...
	'Visible','off');

% Make the menus.
file_menu = uimenu(fig,'Label','File');
uimenu(file_menu,'Label','Open...',...
	'Accelerator','O',...
	'Callback',@open_file)
uimenu(file_menu,'Label','Load from Archive by Date/Time...',...
	'Accelerator','L',...
	'Callback',@open_archive)
archive_menu_item = uimenu(file_menu,...
	'Label','Add Current Data to Archive',...
	'Accelerator','A',...
	'Enable','off',...
	'Callback',@archive_data);
spreadsheet_menu_item = uimenu(file_menu,'Label','Save Spreadsheet...',...
	'Accelerator','S',...
	'Enable','off',...
	'Callback',@save_spreadsheet);
uimenu(file_menu,'Label','Exit',...
	'Separator','on',...
	'Accelerator','Q',...
	'Callback','closereq')

debug_menu = uimenu(fig,'Label','Debug');
uimenu(debug_menu,'Label','Delete Archive',...
	'Callback',@delete_archive)

%--------------------------------------------

% Make the buttons.
butwh = [100 20];
grp1x = 20;
grp1y = 245;
grp2x = 160;
grp2y = 245;

uicontrol('Style','text',...
	'Position',[grp1x grp1y+55 240 15],...
	'String','Current Data:',...
	'HorizontalAlignment','left')

data_info = uicontrol('Style','edit',...
	'Position',[grp1x grp1y+35 240 20],...
	'String','',...
	'BackgroundColor','w',...
	'HorizontalAlignment','left',...
	'Enable','inactive');

but = zeros(1,7);
but(1) = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y butwh],...
	'String','Plot Pressures',...
	'Enable','off',...
	'Callback',@plot_pressures);

but(2) = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-25 butwh],...
	'String','Plot H2O',...
	'Enable','off',...
	'Callback',@plot_h2o);

but(3) = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-50 butwh],...
	'String','Plot PAR',...
	'Enable','off',...
	'Callback',@plot_par);

but(4) = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-75 butwh],...
	'String','Plot Temps & RH',...
	'Enable','off',...
	'Callback',@plot_temps_rh);

but(5) = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-100 butwh],...
	'String','Plot Cal. Gases',...
	'Enable','off',...
	'Callback',@plot_cal_gases);

but(6) = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-125 butwh],...
	'String','Plot Atmos. CO2',...
	'Enable','off',...
	'Callback',@plot_atmos_co2);

but(7) = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-150 butwh],...
	'String','Plot Comp. Temp',...
	'Enable','off',...
	'Callback',@plot_comp_temp);

flux_but = zeros(1,10);
for i = 1:10
	flux_but(i) = uicontrol('Style','pushbutton',...
		'Position',[grp2x grp2y-25*(i-1) butwh],...
		'String',sprintf('Plot Flux-%d',i),...
		'Callback',{@plot_flux,i},...
		'Enable','off');
end

uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-200 butwh],...
	'String','Close All',...
	'Callback',@close_all)

addarch_but = uicontrol('Style','pushbutton',...
	'Position',[grp1x grp1y-225 butwh],...
	'String','Add to Archive',...
	'Callback',@archive_data,...
	'Enable','off');

%--------------------------------------------

% Initialize some data.
results = [];
sys_num = 1;

prefs = readprefs('co2v2_prefs.txt');
record = struct;
home_dir = fileparts(mfilename('fullpath'));
archive_dir = fullfile(home_dir,'archive','');
toc_file = fullfile(archive_dir,'toc.mat');

movegui(fig,[20 -((ss(4) - wh(2) - 56)/4)])
set(fig,'HandleVisibility','off','Visible','on')


	function open_file(varargin)
		[f,p] = uigetfile('*.csv','Select a datalogger file:');
		if isequal(f,0)
			return
		end
		filename = fullfile(p,f);
		
		% Determine system number by looking at first char. of file name.
		sys_num = find(strncmpi(prefs.systems,f,1));
		
		% Read whole file into a matrix, one row per line.
		[data,bad_lines,bad] = read_campbell_v2(filename);
        if ~isempty(bad_lines)
          buttonhit = questdlg( ...
            'Possible bad lines in input file.  Would you like to save a report with the bad line positions?', ...
            'Corrupt file error','Yes','No','Yes');
          if strcmp(buttonhit,'Yes')
            [p,f,e] = fileparts(filename);
            [f,p] = uiputfile('*.txt','Save bad lines report as:',[f '_badlines.txt']);
            if ~isequal(f,0)
              fid = fopen(fullfile(p,f),'w');
              if isequal(fid,-1)
                errordlg('Cannot open file','Error');
              else
                fprintf(fid,'Bad lines found in data file %s\n\n',filename);
                for k=1:length(bad_lines)
                  fprintf(fid,'line %d: %s\n',bad_lines(k),bad{k});
                end
                fclose(fid);
              end
            end
           % return
          end
        end
		
		% Set aside any data before the first state code 111.
		state_111_rows = find(data(:,2) == 111);
		if ~isempty(state_111_rows) && state_111_rows(1) > 1
			pre_data = data(1:state_111_rows(1)-1,:);
			data(1:state_111_rows(1)-1,:) = [];
		else
			pre_data = zeros(0,11);
		end

		% Set aside any data after the final chamber closed state code.
		final_state_rows = find(data(:,2) == ...
			100*prefs.num_chambers(sys_num) + 1);
		post_data = data(final_state_rows(end)+1:end,:);
		data(final_state_rows(end)+1:end,:) = [];
		
		% Process the data, return big structure in results.
		results = process_data(data,prefs,sys_num);
		
		% Construct a record suitable for saving to the archive.
		record = struct('results',results,...
			'starttime',results.wx(1).time,...
			'endtime',results.wx(end).time,...
			'pre_data',pre_data,...
			'pre_data_time',results.wx(1).time - 1/24,...
			'post_data',post_data,...
			'post_data_time',results.wx(end).time + 1/24,...
			'filename',f,...
			'sys_num',sys_num,...
			'system',prefs.systems{sys_num});
		
		% Enable archiving and saving to spreadsheet.
		set(archive_menu_item,'Enable','on')
		set(addarch_but,'Enable','on')
		set(spreadsheet_menu_item,'Enable','on')
		set(data_info,'String',f)
		set(flux_but,'Enable','off')
		set(flux_but(unique([results.closed.chamber])),'Enable','on')
		set(but,'Enable','on')
	end

	function open_archive(varargin)
		% Load table of contents of archive.
		toc = [];
		load(toc_file)
		% Find entries that match desired system name.
		this_sys = strcmp({toc.system},prefs.systems{sys_num});
		min_t = min([toc(this_sys).starttime]);
		max_t = max([toc(this_sys).endtime]);
		if isempty(min_t) || isempty(max_t)
			min_t = min([toc.starttime]);
			max_t = max([toc.endtime]);
		end
		[t,sys_num] = get_date_range([min_t max_t],prefs,toc,sys_num);
		if isempty(t)
			return
		end
		t = sort(t);
		
		% Find archive entries that have dates in specified range.
		halfhour = 0.5/24;
		exclude = t(2) < [toc.starttime] - halfhour | ...
			t(1) > [toc.endtime] + halfhour | ...
			~strcmp({toc.system},prefs.systems{sys_num});
		entries = find(~exclude);
		if isempty(entries)
			return
		end
		
		% Sort them.
		[unused,order] = sort([toc(entries).starttime]);
		entries = entries(order);
		
		% Load each record, concatenating into results as we go.
		record = load(fullfile(archive_dir,toc(entries(1)).matfilename));
		results = record.results;
		for i = 2:length(entries)
			record = load(fullfile(archive_dir,...
				toc(entries(i)).matfilename));
			results.wx = [results.wx,record.results.wx];
			results.closed = [results.closed,record.results.closed];
		end
		
		% Trim parts of records outside specified time interval.
		% Add 59 minutes to end time to cover full hour.
		one_minute = 1/(24*60);
		time_range = t + [0 59*one_minute];
		mid_time = mean(time_range);
		duration = diff(time_range);
		fn = fieldnames(results);
		for i = 1:length(fn)
			exclude = abs([results.(fn{i}).time] - mid_time) > ...
				duration/2 + one_minute;
			results.(fn{i})(exclude) = [];
		end
		
		% Disable saving to archive, enable export to spreadsheet.
		set(archive_menu_item,'Enable','off')
		set(addarch_but,'Enable','off')
		set(spreadsheet_menu_item,'Enable','on')
		fmt = 'mm/dd/yyyy HH:MM';
		msg = sprintf('%s, %s to %s',prefs.systems{sys_num},...
			datestr(t(1),fmt),datestr(t(2),fmt));
		set(data_info,'String',msg)
		set(flux_but,'Enable','off')
		set(flux_but(unique([results.closed.chamber])),'Enable','on')
		set(but,'Enable','on')
	end

	function archive_data(varargin)
		matfilename = sprintf('%s%s.mat',record.system,...
			datestr(record.starttime,'yyyymmddTHHMM'));
		if ~exist(archive_dir,'dir')
			[status,message] = mkdir(archive_dir);
			error(message)
		end
		if exist(toc_file,'file')
			toc = [];
			load(toc_file);
			% Delete entry if it already exists.
			index = find(strcmp(matfilename,{toc.matfilename}));
			toc(index) = [];
		else
			toc = repmat(struct,0,1);
		end
		toc(end+1).matfilename = matfilename;
		toc(end).starttime = record.starttime;
		toc(end).endtime = record.endtime;
		toc(end).pre_data_time = record.pre_data_time;
		toc(end).post_data_time = record.post_data_time;
		toc(end).system = record.system;
		save(fullfile(archive_dir,matfilename),'-struct','record')
		save(toc_file,'toc')
		
		match_pre = strcmp({toc.system},record.system) & ...
			abs(record.pre_data_time - [toc.post_data_time]) < 0.5/24;
		if any(match_pre)
			index = find(match_pre);
			matched_record = load(fullfile(archive_dir,...
				toc(index).matfilename));
			data = [matched_record.post_data;record.pre_data];
			new_results = process_data(data,prefs,record.sys_num);
			new_record.results = new_results;
			new_record.starttime = new_results.wx(1).time;
			new_record.endtime = new_results.wx(end).time;
			new_record.pre_data = [];
			new_record.pre_data_time = NaN;
			new_record.post_data = [];
			new_record.post_data_time = NaN;
			new_record.filename = {matched_record.filename,...
				record.filename};
			new_record.sys_num = record.sys_num;
			new_record.system = record.system;
			matfilename = sprintf('%s%s.mat',new_record.system,...
				datestr(new_record.starttime,'yyyymmddTHHMM'));
			toc = [];
			load(toc_file);
			% Delete entry if it already exists.
			index = find(strcmp(matfilename,{toc.matfilename}));
			toc(index) = [];

			toc(end+1).matfilename = matfilename;
			toc(end).starttime = new_record.starttime;
			toc(end).endtime = new_record.endtime;
			toc(end).pre_data_time = new_record.pre_data_time;
			toc(end).post_data_time = new_record.post_data_time;
			toc(end).system = new_record.system;
			save(fullfile(archive_dir,matfilename),'-struct','new_record')
			save(toc_file,'toc')
		end
		match_post = strcmp({toc.system},record.system) & ...
			abs(record.post_data_time - [toc.pre_data_time]) < 0.5/24;
		if any(match_post)
			index = find(match_post);
			matched_record = load(fullfile(archive_dir,...
				toc(index).matfilename));
			data = [record.post_data;matched_record.pre_data];
			new_results = process_data(data,prefs,record.sys_num);
			new_record.results = new_results;
			new_record.starttime = new_results.wx(1).time;
			new_record.endtime = new_results.wx(end).time;
			new_record.pre_data = [];
			new_record.pre_data_time = NaN;
			new_record.post_data = [];
			new_record.post_data_time = NaN;
			new_record.filename = {record.filename,...
				matched_record.filename};
			new_record.sys_num = record.sys_num;
			new_record.system = record.system;
			matfilename = sprintf('%s%s.mat',new_record.system,...
				datestr(new_record.starttime,'yyyymmddTHHMM'));
			toc = [];
			load(toc_file);
			% Delete entry if it already exists.
			index = find(strcmp(matfilename,{toc.matfilename}));
			toc(index) = [];

			toc(end+1).matfilename = matfilename;
			toc(end).starttime = new_record.starttime;
			toc(end).endtime = new_record.endtime;
			toc(end).pre_data_time = new_record.pre_data_time;
			toc(end).post_data_time = new_record.post_data_time;
			toc(end).system = new_record.system;
			save(fullfile(archive_dir,matfilename),'-struct','new_record')
			save(toc_file,'toc')
		end
		% Disable saving to archive.
		set(archive_menu_item,'Enable','off')
		set(addarch_but,'Enable','off')
	end

	function save_spreadsheet(varargin)
		start_datestr = datestr(results.wx(1).time,'yyyymmddTHH');
		hours = round((results.wx(end).time - results.wx(1).time)*24);
		default_filename = sprintf('%s_%s_%d.csv',...
			prefs.systems{sys_num},start_datestr,hours);
		[f,p] = uiputfile(default_filename,'Save CSV file as:');
		if isequal(f,0)
			return
		end
		csv_file = fullfile(p,f);
		
		% Convert structure to cell array.
		wx_cell = permute(struct2cell(results.wx),[3 1 2]);
		% Replace NaNs with -6999.
		nan_loc = find(isnan(cell2mat(wx_cell(:,3:end)))) + ...
			2*size(wx_cell,1);
		wx_cell(nan_loc) = {-6999};
		% Reformat date number into separate date and time strings and put
		% in first two cell columns.
		time = [wx_cell{:,2}];
		wx_cell(:,1) = cellstr(datestr(time,'mm/dd/yyyy'));
		wx_cell(:,2) = cellstr(datestr(time,'HH:MM'));
		% Transpose to get proper order for fprintf.
		wx_cell = wx_cell';
		% Compute header, header format and data format.
		wx_headers = fieldnames(results.wx);
		wx_headers{1} = 'date';
		num_wx_cols = length(wx_headers);
		wx_hdr_fmt = [repmat('"%s",',1,num_wx_cols-1),'"%s"\n'];
		wx_data_fmt = '%s,%s,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g\n';
		
		% Convert structure to cell array.
		closed_cell = permute(struct2cell(results.closed),[3 1 2]);
		% Replace NaNs with -6999.
		nan_loc = find(isnan(cell2mat(closed_cell(:,4:end)))) + ...
			3*size(closed_cell,1);
		closed_cell(nan_loc) = {-6999};
		% Put chamber number in first column and sort by it.
		closed_cell(:,1) = closed_cell(:,2);
		[unused,order] = sort([closed_cell{:,1}]);
		closed_cell = closed_cell(order,:);
		% Reformat date number into separate date and time strings and put
		% in first two cell columns.
		time = [closed_cell{:,3}];
		closed_cell(:,2) = cellstr(datestr(time,'mm/dd/yyyy'));
		closed_cell(:,3) = cellstr(datestr(time,'HH:MM'));
		% Transpose to get proper order for fprintf.
		closed_cell = closed_cell';
		
		% Compute header, header format and data format.
		closed_headers = fieldnames(results.closed);
		closed_headers(1:3) = {'chamber','date','time'};
		num_closed_cols = length(closed_headers);
		closed_hdr_fmt = [repmat('"%s",',1,num_closed_cols-1),'"%s"\n'];
		closed_data_fmt = ['%g,%s,%s,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,',...
			'%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g\n'];
		
		% Open file, write data, close file.
		fid = fopen(csv_file,'wt');
		if fid == -1
			uiwait(errordlg({'Unable to open CSV file for writing.',...
				'Disk is read-only or folder is write protected.'}))
			return
		end
		fprintf(fid,wx_hdr_fmt,wx_headers{:});
		fprintf(fid,wx_data_fmt,wx_cell{:});
		fprintf(fid,'\n');
		fprintf(fid,closed_hdr_fmt,closed_headers{:});
		fprintf(fid,closed_data_fmt,closed_cell{:});
		fclose(fid);
	end

	function delete_archive(varargin)
		answer = questdlg({'Are you sure you want to delete the Archive?',...
			'This operation cannot be undone.'},'','Yes','No','No');
		if strcmp(answer,'Yes')
			delete(fullfile(archive_dir,'*'))
			toc = [];
		end
	end

	function plot_pressures(varargin)
		plot_name = 'Pressures';
		x = [results.wx.time]';
		y1 = [results.wx.BP]';
		y2 = [results.wx.P_tank]';
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			[ax,h1,h2] = plotyy(x,y1,x,y2);
			datetick(ax(1))
			datetick(ax(2))
			legend([h1;h2],{'BP','P_{tank}'},'Location','Best')
			xlabel('Date')
			ylabel(ax(1),'BP (mb)')
			ylabel(ax(2),'Tank pressure (psi)')
			title('Pressures')
		else
			figure(plotfig)
		end
	end

	function plot_h2o(varargin)
		plot_name = 'H2O';
		x = [results.wx.time];
		y1 = 100*[results.wx.H2O_1cm];
		y2 = 100*[results.wx.H2O_5cm];
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			h = plot(x,y1,x,y2);
			datetick
			legend(h,{'1 cm','5 cm'},'Location','East')
			xlabel('Date')
			ylabel('H_2O (%)')
			title('Volumetric Water Content')
		else
			figure(plotfig)
		end
	end

	function plot_par(varargin)
		plot_name = 'PAR';
		x = [results.wx.time]';
		y1 = [results.wx.PAR_A]';
		y2 = [results.wx.PAR_B]';
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			h = plot(x,y1,x,y2);
			datetick
			legend(h,{'PAR-A','PAR-B'},'Location','Best')
			xlabel('Date')
			ylabel('PAR (\mumols photons/m^2/s)')
			title('Photosynthetically Active Radiation')
		else
			figure(plotfig)
		end
	end

	function plot_temps_rh(varargin)
		plot_name = 'Temps_RH';
		x = [results.wx.time]';
		y1 = [results.wx.T_air]';
		y2 = [results.wx.T_crust]';
		y12 = [y1,y2];
		y3 = [results.wx.RH]';
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			[ax,h_temp] = plotyy(x,y12,x,y3);
			datetick(ax(1))
			datetick(ax(2))
			legend(h_temp,{'T_{air}','T_{crust}'},'Location','Best')
			xlabel(ax(1),'Date')
			ylabel(ax(1),'Temperature (\circC)')
			ylabel(ax(2),'RH (%)','Color','b')
			title('Air and Crust Temperatures and RH')
		else
			figure(plotfig)
		end
	end

	function plot_cal_gases(varargin)
		plot_name = 'Calibration_gases';
		x = [results.wx.time];
		y1 = [results.wx.CO2_zero];
		y2 = [results.wx.CO2_span];
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			h = plot(x,y1,x,y2);
			datetick
			legend(h,{'Zero gas','Span gas'},'Location','East')
			xlabel('Date')
			ylabel('Gas concentration (ppm)')
			title('Zero and Span Gas values')
		else
			figure(plotfig)
		end
	end

	function plot_atmos_co2(varargin)
		plot_name = 'atmos_co2';
		x = [results.wx.time]';
		y = [results.wx.CO2];
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			plot(x,y);
			datetick
			xlabel('Date')
			ylabel('CO_2 concentration (ppm)')
			title('Atmospheric CO_2')
		else
			figure(plotfig)
		end
	end

	function plot_comp_temp(varargin)
		plot_name = 'compressor_temp';
		x = [results.wx.time]';
		y = [results.wx.T_compressor];
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			plot(x,y);
			datetick
			xlabel('Date')
			ylabel('Temperature (\circC)')
			title('Compressor Temperature')
		else
			figure(plotfig)
		end
	end

	function plot_flux(varargin)
		chamber = varargin{3};
		x = [results.closed.time];
		y = [results.closed.flux];
		index = find([results.closed.chamber] == chamber);
		x = x(index);
		y = y(index);
		plot_name = sprintf('Flux-%d',chamber);
		plotfig = findobj('Type','figure','Tag',['co2v2_',plot_name],...
			'UserData',fig);
		if isempty(plotfig)
			plotfig = figure('Tag',['co2v2_',plot_name],...
				'UserData',fig,...
				'IntegerHandle','off',...
				'NumberTitle','off',...
				'Name',plot_name,...
				'HandleVisibility','callback');
			whitebg(plotfig,'w')
			plot(x,y)
			% Fixed range for y-axis.
			ylim([-1.0 5.0])
			datetick
			xlabel('Date')
			ylabel('flux (\mumols CO_2/m^2/s)')
			title(sprintf('CO_2 flux, Chamber %d',chamber))
		else
			figure(plotfig)
		end
	end

	function close_all(varargin)
		figs = findobj('Type','figure','UserData',fig);
		tags = get(figs,'Tag');
		delete(figs(strncmp(tags,'co2v2_',6)))
	end

end



%-----------------------------------------------------------------
function out = process_data(data,prefs,sys_num)

crust_area = 0.108; % m^2
R = 83.188; % gas constant, L-mb/mol/K
Pf = prefs.Pf(sys_num);

% Separate data into blocks by state code.
index1 = [1;find(diff(data(:,2)))+1];
index2 = [find(diff(data(:,2)));size(data,1)];
num_blocks = length(index1);
blocks_per_cycle = 3 + 2*prefs.num_chambers(sys_num);
max_cycles = ceil(1.1*num_blocks/blocks_per_cycle);
% Initialize out and counters
out = struct;
out.wx(max_cycles) = struct('state',[],'time',[],'BP',[],'H2O_1cm',[],...
	'H2O_5cm',[],'PAR_A',[],'PAR_B',[],'P_tank',[],'CO2',[],'RH',[],...
	'T_air',[],'T_crust',[],'CO2_zero',[],'CO2_span',[],...
	'span_gas_conc',[],'T_compressor',[]);
out.closed(max_cycles*prefs.num_chambers(sys_num)) = struct('state',[],...
	'chamber',[],'time',[],'state_code',[],'chloc',[],'BP',[],...
	'CO2_start',[],'CO2_end',[],'RH_inline',[],'T_inline',[],'flow',[],...
	'T_air_start',[],'T_air_end',[],'T_air_max',[],'T_air_min',[],...
	'T_crust_start',[],'T_crust_end',[],'T_crust_max',[],...
	'T_crust_min',[],'PAR',[],'flux',[],'H2O_1cm',[],'H2O_5cm',[],...
	'vol',[],'span_gas_conc',[]);
wx_ctr = 0;
closed_ctr = 0;
wb = waitbar(0,'Processing datalogger file...');
try
  set(wb,'HandleVisibility','off')
  for i = 1:num_blocks
      block = make_block_struct(data(index1(i):index2(i),:));
      switch block.state
          case 'baseline_weather'
              wx_ctr = wx_ctr + 1;
              out.wx(wx_ctr) = block;
          case 'zero_gas'
              block.time = out.wx(wx_ctr).time;
  % 			actual_RH = 0;
              CO2_zero = last_non_NaN(block.CO2_raw);
              out.wx(wx_ctr).CO2_zero = CO2_zero;
          case 'span_gas'
              block.time = out.wx(wx_ctr).time;
  % 			actual_RH = 0;
              CO2_span = last_non_NaN(block.CO2_raw);
              IBP_span = last_non_NaN(block.BP_inline);
              out.wx(wx_ctr).CO2_span = CO2_span;
              % Compute CO2 calibration polynomial.
              span_gas_conc = get_span_gas_conc(prefs,sys_num,block.time);
              cal_poly = polyfit([CO2_zero CO2_span],[0 span_gas_conc],1);
          case 'open'
              % block.time contains just the time-of-day; add to it the day
              % (integer part of time of latest weather record).  If
              % block.time is less than the time-of-day of the latest weather
              % record then block.time must be in the next day so add 1 more.
              if block.time < rem(out.wx(wx_ctr).time,1)
                  block.time = floor(out.wx(wx_ctr).time) + block.time + 1;
              else
                  block.time = floor(out.wx(wx_ctr).time) + block.time;
              end
              span_gas_conc = get_span_gas_conc(prefs,sys_num,block.time);
              % Correct CO2 and apply calibration polynomial.
              [CO2_open,w_open] = correct_CO2(last_non_NaN(block.CO2_raw),...
                  last_non_NaN(block.BP_inline),...
                  IBP_span,...
                  last_non_NaN(block.RH_inline),...
                  last_non_NaN(block.T_inline),cal_poly,Pf);
              if block.chamber == 1
                  CO2_atmos = correct_CO2(last_non_NaN(block.CO2_raw),...
                      last_non_NaN(block.BP_inline),...
                      IBP_span,...
                      last_non_NaN(block.RH_inline),...
                      last_non_NaN(block.T_inline),cal_poly,Pf,0);
                  out.wx(wx_ctr).CO2 = CO2_atmos;
                  out.wx(wx_ctr).RH = mean_last_4(block.RH_inline);
                  out.wx(wx_ctr).T_air = mean_last_4(block.T_air);
                  out.wx(wx_ctr).T_crust = mean_last_4(block.T_crust);
                  out.wx(wx_ctr).span_gas_conc = span_gas_conc;
                  out.wx(wx_ctr).T_compressor = block.T_compressor;
              end
          case 'closed'
              % block.time contains just the time-of-day; add to it the day
              % (integer part of time of latest weather record).  If
              % block.time is less than the time-of-day of the latest weather
              % record then block.time must be in the next day so add 1 more.
              if block.time < rem(out.wx(wx_ctr).time,1)
                  block.time = floor(out.wx(wx_ctr).time) + block.time + 1;
              else
                  block.time = floor(out.wx(wx_ctr).time) + block.time;
              end
              CO2_closed = correct_CO2(block.CO2_raw,block.BP_inline,...
                  IBP_span,block.RH_inline,block.T_inline,...
                  cal_poly,Pf,w_open);
              [slope, T_air_avg] = compute_slope(CO2_closed,block.T_air);
              chamber_vol = prefs.chamber_vol(block.chamber,sys_num);
              flux = slope*out.wx(wx_ctr).BP*chamber_vol/(crust_area*R*...
                  (T_air_avg + 273.15));
              ch_loc = last_non_NaN(block.ch_loc);
              [unused,actual_chamber] = ...
                  min(abs(prefs.chloc_table(:,sys_num) - ch_loc));
              if prefs.chloc_enable && block.chamber ~= actual_chamber
                  flux = NaN;
              end

              closed_ctr = closed_ctr + 1;
              out.closed(closed_ctr).state = block.state;
              out.closed(closed_ctr).chamber = block.chamber;
              out.closed(closed_ctr).time = block.time;
              out.closed(closed_ctr).state_code = 100*block.chamber + 1;
              out.closed(closed_ctr).chloc = ch_loc;
              out.closed(closed_ctr).BP = mean_nan(block.BP_inline);
              out.closed(closed_ctr).CO2_start = CO2_closed(1);
              out.closed(closed_ctr).CO2_end = CO2_closed(end);
              out.closed(closed_ctr).RH_inline = mean_nan(block.RH_inline);
              out.closed(closed_ctr).T_inline = mean_nan(block.T_inline);
              out.closed(closed_ctr).flow = mean_nan(block.flow);
              out.closed(closed_ctr).T_air_start = block.T_air(1);
              out.closed(closed_ctr).T_air_end = block.T_air(end);
              out.closed(closed_ctr).T_air_max = max(block.T_air);
              out.closed(closed_ctr).T_air_min = min(block.T_air);
              out.closed(closed_ctr).T_crust_start = block.T_crust(1);
              out.closed(closed_ctr).T_crust_end = block.T_crust(end);
              out.closed(closed_ctr).T_crust_max = max(block.T_crust);
              out.closed(closed_ctr).T_crust_min = min(block.T_crust);
              out.closed(closed_ctr).PAR = mean_nan(block.PAR);
              out.closed(closed_ctr).flux = flux;
              out.closed(closed_ctr).H2O_1cm = out.wx(wx_ctr).H2O_1cm;
              out.closed(closed_ctr).H2O_5cm = out.wx(wx_ctr).H2O_5cm;
              out.closed(closed_ctr).vol = chamber_vol;
              out.closed(closed_ctr).span_gas_conc = span_gas_conc;
      end
      waitbar(i/num_blocks,wb)
  end
  % Trim unused structures.
  out.wx(wx_ctr+1:end) = [];
  out.closed(closed_ctr+1:end) = [];
  close(wb)
catch ME
  close(wb)
  disp(['Error:' ME.message])
  errordlg('Error encountered while processing data.  Possible badly formatted file.','Bad File Error');
end

end


%-----------------------------------------------------------------
function [CO2cspwd,w] = correct_CO2(CO2unc,IBP,IBPspan,IRH,ITemp,poly,Pf,w_open)

% Span correction.
CO2cs = polyval(poly,CO2unc);

% Pressure correction.
CO2csp = CO2cs + Pf*(IBP/IBPspan - 1);

% Band broadening due to water vapor.
w = 1000*(IRH/100).*(0.61083*10.^((7.6448*ITemp)./(242.62+ITemp)))./(IBP/10);
CO2cspw = (1 + 0.5*w/1000).*CO2csp.*(1 - ((0.5*w/1000).*...
	(((6606.6 + 1.4306*CO2csp.^1.5)./(6606.6 + CO2csp.^1.5)) + ...
	CO2csp*0.00022464)));

% Correction of CO2 for water vapor dilution.
if nargin < 8
	CO2cspwd = CO2cspw;
else
	CO2cspwd = CO2cspw.*(1 - w_open/1000)./(1 - w/1000);
end

end


%-----------------------------------------------------------------
function [slope, T_air_avg] = compute_slope(CO2,T_air)
if sum(~isnan(CO2)) < 3
	slope = NaN;
	T_air_avg = NaN;
	return
end
t = 10*(0:length(CO2)-1)'; % sample time in seconds
% Eliminate bad CO2 values.
ok = ~isnan(CO2);
t = t(ok);
CO2 = CO2(ok);
T_air = T_air(ok);
% Compute derivatives
d_CO2 = gradient(CO2,t);           % derivative of CO2
%s_d_CO2 = supersmoother(t,d_CO2);  % smoothed derivative of CO2
s_d_CO2 = supsmu(t,d_CO2);  % smoothed derivative of CO2
d_s_d_CO2 = gradient(s_d_CO2,t);   % curvature of CO2 (second derivative)

abs_curvature = abs(d_s_d_CO2);
max_abs_curvature = max(abs_curvature);
if max_abs_curvature == 0
	max_abs_curvature = 1;
end
weight = (1 - abs_curvature/max_abs_curvature).^2;
weight(1:4) = 0;
sum_weight = sum(weight);
slope = sum(weight.*s_d_CO2)/sum_weight;
T_air_avg = sum(weight.*T_air)/sum_weight;
end


%-----------------------------------------------------------------
function block = make_block_struct(data)
state_code = data(1,2);
switch state_code
	case 111
		block.state = 'baseline_weather';
		year = data(1,3);
		day_of_year = data(1,4);
		hour_minute = data(1,5);
		minute = rem(hour_minute,100);
		hour = round((hour_minute - minute)/100);
		block.time = datenum(year, 1, day_of_year, hour, minute, 0);
		block.BP = data(1,6);
		block.H2O_1cm = data(1,7);
		block.H2O_5cm = data(1,8);
		block.PAR_A = data(1,9);
		block.PAR_B = data(1,10);
		block.P_tank = data(1,11);
		block.CO2 = NaN;
		block.RH = NaN;
		block.T_air = NaN;
		block.T_crust = NaN;
		block.CO2_zero = NaN;
		block.CO2_span = NaN;
		block.span_gas_conc = NaN;
		block.T_compressor = NaN;

	case 222
		block.state = 'zero_gas';
		block.chamber = 0;
		block.time = NaN;
		block.BP_inline = data(:,3);
		block.CO2_raw = data(:,4);
		block.RH_inline = data(:,5);
		block.T_inline = data(:,6);
		block.flow = data(:,7);
		block.ch_loc = data(:,8);
		block.T_air = data(:,9);
		block.T_crust = data(:,10);
		block.PAR = data(:,11);
		block.final_CO2 = NaN;

	case 333
		block.state = 'span_gas';
		block.chamber = 0;
		block.time = NaN;
		block.BP_inline = data(:,3);
		block.CO2_raw = data(:,4);
		block.RH_inline = data(:,5);
		block.T_inline = data(:,6);
		block.flow = data(:,7);
		block.ch_loc = data(:,8);
		block.T_air = data(:,9);
		block.T_crust = data(:,10);
		block.PAR = data(:,11);
		block.final_CO2 = NaN;

	otherwise
		if rem(state_code,100) == 0
			block.state = 'open';
		else
			block.state = 'closed';
			block.flux = NaN;
		end
		block.chamber = floor(state_code/100);

		hour_minute = data(1,3);
		minute = rem(hour_minute,100);
		hour = round((hour_minute - minute)/100);
		block.time = datenum(0, 0, 0, hour, minute, 0);
		block.ch_loc = data(1,4);
		block.T_compressor = data(1,5);
		if sum(~isnan(data(end,:))) == 4
			data(end,:) = [];
		end
		block.BP_inline = data(2:end,3);
		block.CO2_raw = data(2:end,4);
		block.RH_inline = data(2:end,5);
		block.T_inline = data(2:end,6);
		block.flow = data(2:end,7);
		block.ch_loc = data(2:end,8);
		block.T_air = data(2:end,9);
		block.T_crust = data(2:end,10);
		block.PAR = data(2:end,11);
end
end

%------------------------------------------------------------------
function [data,bad_lines,bad] = read_campbell_v2(filename)
%Read campbell data files (be robust to partially corrupted files)
lines_per_block = 2501;
bad = [];
bad_lines = [];

% Open file and get its length.
fid = fopen(filename,'rt');
fseek(fid,0,'eof');
flen = ftell(fid);
frewind(fid)

% Get blocks of file until we reach the end.
block_counter = 0;
data_blocks = cell(1,2000);
lines = textscan(fid,'%s');
lines = lines{1}; % Unpack
fclose(fid);

% Interpret each line
data = cell(size(lines));
for i=1:length(lines)
   data(i) = textscan(lines{i},'%f','Delimiter',' ,\t');
end

% Work around bug in textscan in R2013b (an empty last field is ignored)
len = cellfun(@length,data);
short_lines = find(len == 10);
for i=1:length(short_lines)
  data{short_lines(i)}(11) = NaN;
end

% Automatically pad lines with 4 or 5 elements (these are transition lines)
short_lines = find(len == 4 | len == 5);
for i=1:length(short_lines)
  data{short_lines(i)} = [data{short_lines(i)};NaN(11-len(short_lines(i)),1)];
end

% Correctly formatted Campbell files have 11 columns.  
% Capture bad lines info for user
len = cellfun(@length,data);
bad_lines = len ~= 11;
if any(bad_lines)
  % Truncate long lines
  k = find(len > 11);
  for i=1:length(k)
    data{k(i)} = data{k(i)}(1:11);
  end
  
  % Pad short lines
  k = find(len < 11);
  for i=1:length(k)
    data{k(i)} = [data{k(i)};NaN(11-len(k(i)),1)];
  end
end

% Look for bad sensor readings
bad_sensor = false(size(data));
for i=1:length(data)
  bad_sensor(i) = any(data{i}==-99999 | data{i}==6999);
end

if any(bad_sensor)
  % Auto transform 6999 readings to NaN (for process_data)
  k = find(bad_sensor);
  for i=1:length(k)
    d = data{k(i)};
    loc = d == 6999;
    if any(loc) && d(2) == 111 % Transform 6999 only when statecode is 111
      d(loc) = NaN;
      data{k(i)} = d;
      bad_sensor(k(i)) = any(d==-99999 | d==6999); % Update bad_sensor
    end
  end
end

% Create outputs
bad_lines = find(bad_lines | bad_sensor);
bad = lines(bad_lines);

% Concatenate into matrix
for i=1:length(data) 
  data{i} = data{i}'; % Doing the transpose first saves memory
end
data = cat(1,data{:});
end

%-----------------------------------------------------------------
function data = read_campbell(filename)

lines_per_block = 500;
data_blocks = cell(1,2000);
block_counter = 0;

% Open file and get its length.
fid = fopen(filename,'rt');
fseek(fid,0,'eof');
flen = ftell(fid);
frewind(fid)

% Get blocks of file until we reach the end.
while ftell(fid) < flen
	block = textscan(fid, '%f%f%f%f%f%f%f%f%f%f%f', lines_per_block, ...
		'delimiter', ' ,\t');
	
	% Look at lengths of columns.  Extend any short ones with NaNs.
	lengths = cellfun('length', block);
	longest = max(lengths);
	short = find(lengths < longest);
	for col = 1:length(short)
		block{short(col)}(end+1:longest) = NaN;
	end
	
	% Concatenate block into a matrix and append to end of data_blocks.
	block_counter = block_counter + 1;
	data_blocks{block_counter} = [block{:}];
end
fclose(fid);

% Concatenate data_blocks into one big matrix.
data = cat(1,data_blocks{:});
end


%-----------------------------------------------------------------
function conc = get_span_gas_conc(prefs,sys_num,time)
index = find(prefs.span_gas(sys_num).t < time);
if isempty(index)
	conc = 350;
else
	conc = prefs.span_gas(sys_num).conc(index(end));
end
end


%-----------------------------------------------------------------
function m = mean_nan(x)
% Average of non-NaN elements of a vector.
numbers = ~isnan(x);
n = sum(numbers);
if n == 0
	n = NaN;
end
m = sum(x(numbers))/n;
end


%-----------------------------------------------------------------
function m = mean_last_4(x)
% Average of last four elements of a vector.
if length(x) < 4
	m = mean_nan(x);
else
	m = mean_nan(x(end-3:end));
end
end


%-----------------------------------------------------------------
function y = last_non_NaN(x)
% Last element of a vector that is not a NaN.
x(isnan(x)) = [];
if isempty(x)
	y = NaN;
else
	y = x(end);
end
end


%-----------------------------------------------------------------
function prefs = readprefs(filename)
fid = fopen(filename);
fseek(fid,0,'eof');
file_length = ftell(fid);
frewind(fid);

while ftell(fid) < file_length
	item = textscan(fid,'%s',1,'whitespace','\t');
	switch item{1}{1}
		case 'systems'
			% will support up to 10 systems, change if necessary
			max_systems = 10;
			v = textscan(fid,repmat('%s',1,max_systems),1);
			v = [v{:}];
			prefs.systems = v(cellfun('length',v) ~= 0);
			num_systems = length(prefs.systems);
			
		case 'pressure factors'
			v = textscan(fid,repmat('%f',1,num_systems),'headerlines',1);
			prefs.Pf = [v{:}];
			
		case 'chamber volumes'
			v = textscan(fid,repmat('%f',1,num_systems),'headerlines',2);
			prefs.chamber_vol = [v{:}]/1000; % chamber volumes in liters
			prefs.num_chambers = sum(prefs.chamber_vol ~= 0);
			
		case 'chloc table'
			v = textscan(fid,repmat('%f',1,num_systems),'headerlines',2);
			prefs.chloc_table = [v{:}];
			prefs.chloc_enable = true;
			
		case 'chloc table enable'
			v = textscan(fid,repmat('%f',1,num_systems),'headerlines',2);
			prefs.chloc_table = [v{:}];
			prefs.chloc_enable = true;
			
		case 'chloc table disable'
			v = textscan(fid,repmat('%f',1,num_systems),'headerlines',2);
			prefs.chloc_table = [v{:}];
			prefs.chloc_enable = false;
			
		case 'tank changes'
			v = textscan(fid,'%s%s%s%f','headerlines',2);
			t = datenum(v{2}) + datenum(v{3}) - datenum('00:00');
			[ts,order] = sort(t);
			systems = v{1}(order);
			conc = v{4}(order);
			for i = 1:num_systems
				this_system_flag = strcmpi(systems,prefs.systems{i});
				prefs.span_gas(i).t = ts(this_system_flag);
				prefs.span_gas(i).conc = conc(this_system_flag);
			end
	end
end

fclose(fid);
end


%-----------------------------------------------------------------
function [t,sys_num] = get_date_range(t_range,prefs,toc,sys_num)
gray = get(0,'DefaultUIControlBackgroundColor');
fig = figure('Position',[0 0 360 320],...
	'Toolbar','none',...
	'Menubar','none',...
	'IntegerHandle','off',...
	'NumberTitle','off',...
	'Name','Enter a date/time range:',...
	'Color',gray,...
	'Visible','off');
movegui(fig,'center')

common.Style = 'text';
common.BackgroundColor = gray;
common.HorizontalAlignment = 'right';
uicontrol(common,'Position',[55 277 60 20],'String','System:')

common.Style = 'popupmenu';
common.BackgroundColor = 'w';
common.HorizontalAlignment = 'left';
popup_menu = uicontrol(common,'Position',[120 280 120 20],...
	'String',prefs.systems,'Value',sys_num,'Callback',@popup);

common.Style = 'text';
common.BackgroundColor = gray;
common.HorizontalAlignment = 'left';
uicontrol(common,'Position',[20 240 100 20],'String','Start date/time:')

common.Style = 'text';
common.BackgroundColor = gray;
common.HorizontalAlignment = 'left';
uicontrol(common,'Position',[ 40 220 60 20],'String','Year')
uicontrol(common,'Position',[120 220 60 20],'String','Month')
uicontrol(common,'Position',[200 220 60 20],'String','Day')
uicontrol(common,'Position',[280 220 60 20],'String','Hour')

time1edits = zeros(1,4);
common.Style = 'edit';
common.BackgroundColor = 'w';
common.HorizontalAlignment = 'left';
time1edits(1) = uicontrol(common,'Position',[ 40 200 60 20]);
time1edits(2) = uicontrol(common,'Position',[120 200 60 20]);
time1edits(3) = uicontrol(common,'Position',[200 200 60 20]);
time1edits(4) = uicontrol(common,'Position',[280 200 60 20]);


common.Style = 'text';
common.BackgroundColor = gray;
common.HorizontalAlignment = 'left';
uicontrol(common,'Position',[20 150 100 20],'String','End date/time:')

common.Style = 'text';
common.BackgroundColor = gray;
common.HorizontalAlignment = 'left';
uicontrol(common,'Position',[ 40 130 60 20],'String','Year')
uicontrol(common,'Position',[120 130 60 20],'String','Month')
uicontrol(common,'Position',[200 130 60 20],'String','Day')
uicontrol(common,'Position',[280 130 60 20],'String','Hour')

time2edits = zeros(1,4);
common.Style = 'edit';
common.BackgroundColor = 'w';
common.HorizontalAlignment = 'left';
time2edits(1) = uicontrol(common,'Position',[ 40 110 60 20]);
time2edits(2) = uicontrol(common,'Position',[120 110 60 20]);
time2edits(3) = uicontrol(common,'Position',[200 110 60 20]);
time2edits(4) = uicontrol(common,'Position',[280 110 60 20]);

common.Style = 'pushbutton';
common.BackgroundColor = gray;
common.HorizontalAlignment = 'center';
uicontrol(common,'Position',[90 40 80 30],...
	'String','Cancel',...
	'Callback',@cancel)
uicontrol(common,'Position',[190 40 80 30],...
	'String','OK',...
	'Callback',@ok)

vec1 = datevec(t_range(1));
vec2 = datevec(t_range(2));
init1 = textscan(sprintf('%d ',vec1(1:4)),'%s');
init2 = textscan(sprintf('%d ',vec2(1:4)),'%s');
set(time1edits,{'String'},init1{1})
set(time2edits,{'String'},init2{1})

set(fig,'Visible','on')

uiwait


	function popup(varargin)
		sys_num = get(popup_menu,'Value');
		this_sys = strcmp({toc.system},prefs.systems{sys_num});
		min_t = min([toc(this_sys).starttime]);
		max_t = max([toc(this_sys).endtime]);
		if ~isempty(min_t) && ~isempty(max_t)
			vec1 = datevec(min_t);
			vec2 = datevec(max_t);
			init1 = textscan(sprintf('%d ',vec1(1:4)),'%s');
			init2 = textscan(sprintf('%d ',vec2(1:4)),'%s');
			set(time1edits,{'String'},init1{1})
			set(time2edits,{'String'},init2{1})
		else
			set(time1edits,{'String'},{''})
			set(time2edits,{'String'},{''})
		end
	end

	function cancel(varargin)
		t = [];
		delete(fig)
	end

	function ok(varargin)
		t1c = get(time1edits,'String');
		t2c = get(time2edits,'String');
		t1v = zeros(1,6);
		t1v(1:4) = sscanf(sprintf('%s ',t1c{:}),'%d')';
		t2v = zeros(1,6);
		t2v(1:4) = sscanf(sprintf('%s ',t2c{:}),'%d')';
		t = [datenum(t1v) datenum(t2v)];
		delete(fig)
	end

end
