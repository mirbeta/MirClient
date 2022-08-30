{$INCLUDE ..\..\Compilers.inc}
unit Summary;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.ComCtrls, VCL.Buttons,
     VCL.Grids;
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, ImgList, ComCtrls, ExtCtrls, StdCtrls, Buttons, Grids;
     {$ENDIF}

type
  Twnd_msi_Summary = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    bSave: TButton;
    sd: TSaveDialog;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel3: TPanel;
    HWList: TListView;
    Panel4: TPanel;
    SWList: TListView;
    Panel5: TPanel;
    OSList: TListView;
    TabSheet4: TTabSheet;
    Panel6: TPanel;
    NetList: TListView;
    Header: TPanel;
    OverviewIcon: TImage;
    lMachine: TLabel;
    lCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure HWListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure HWListColumnClick(Sender: TObject; Column: TListColumn);
    procedure HWListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure HWListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  private
  public
    procedure ExportCSV(AList: TListView; AFilename: string);
    procedure RefreshData;
  end;

procedure ShowSummaryReport;

var
  wnd_msi_Summary: Twnd_msi_Summary;

implementation

uses Main, Viewer, MSI_CPU, MiTeC_WinIOCTL, MiTeC_CtrlRtns, MSI_Network;

{$R *.dfm}

procedure ShowSummaryReport;
begin
  with Twnd_msi_Summary.Create(Application.Mainform) do
    try
      Screen.Cursor:=crHourglass;
      try
        RefreshData;
      finally
        Screen.Cursor:=crDefault;
      end;
      ShowModal;
    finally
      Free;
    end;
end;

{ Twnd_msi_Summary }

procedure Twnd_msi_Summary.bSaveClick(Sender: TObject);
begin
  case pc.ActivePage.PageIndex of
    0: sd.FileName:='HW.csv';
    1: sd.FileName:='SW.csv';
    2: sd.FileName:='OS.csv';
    3: sd.FileName:='Network.csv';
  end;
  if not sd.Execute then
    Exit;
  case pc.ActivePage.PageIndex of
    0: ExportCSV(HWList,sd.FileName);
    1: ExportCSV(SWList,sd.FileName);
    2: ExportCSV(OSList,sd.FileName);
    3: ExportCSV(NetList,sd.FileName);
  end;
end;

procedure Twnd_msi_Summary.ExportCSV;
var
  i,j: Integer;
  s,f: string;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    s:='';
    for i:=0 to AList.Columns.Count-1 do
      s:=s+AList.Column[i].Caption+';';
    Setlength(s,Length(s)-1);
    sl.Add(s);
    for j:=0 to AList.Items.Count-1 do begin
      s:='';
      for i:=0 to AList.Columns.Count-1 do begin
        if i=0 then
          f:=AList.Items[j].Caption
        else
          f:=AList.Items[j].SubItems[i-1];
        {if AList.Column[i].Alignment<>taRightJustify then
          f:='"'+f+'"';}
        s:=s+f+';'
      end;
      Setlength(s,Length(s)-1);
      sl.Add(s);
    end;
    sl.SaveToFile(AFilename);
  finally
    sl.Free;
  end;
end;

procedure Twnd_msi_Summary.FormCreate(Sender: TObject);
begin
  pc.ActivePage:=Tabsheet1;
  {$IFDEF THEMESUPPORT}
  Header.ParentBackground:=False;
  {$ENDIF}
end;

procedure Twnd_msi_Summary.HWListAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Twnd_msi_Summary.HWListAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=SubItem+1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Twnd_msi_Summary.HWListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  TListView(Sender).SortType:=stNone;
  if Column.Index+1<>abs(TListView(Sender).Tag) then
    TListView(Sender).Tag:=Column.Index+1
  else
    TListView(Sender).Tag:=-TListView(Sender).Tag;
  TListView(Sender).SortType:=stText;
end;

procedure Twnd_msi_Summary.HWListCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  Compare:=ListView_CustomSort(Item1,Item2,abs(TListView(Sender).Tag)-1);
  if TListView(Sender).Tag<0 then
    Compare:=-Compare;
end;

procedure Twnd_msi_Summary.RefreshData;
var
  i,j: Integer;
  s1,s2,osn: string;
  t,c,cn,dn,tn: Integer;
  l: TListItem;
begin
  with wnd_msi_Main do begin
    lCount.Caption:=Format('Count: %d',[MDIChildCount]);
    for i:=0 to MDIChildCount-1 do begin
      with Tmdi_msi_Viewer(MDIChildren[i]) do begin
        with HWList.Items.Add, SIC do begin
          Caption:=Machine.MachineName;
          SubItems.Add(IntToStr(CPU.CPUPhysicalCount));;
          SubItems.Add(cVendorNames[CPU.Vendor].Prefix);
          SubItems.Add(CPU.CPUName);
          SubItems.Add(IntToStr(CPU.Frequency));
          t:=0;
          for j:=0 to Machine.SMBIOS.MemoryModuleCount-1 do
            t:=t+Machine.SMBIOS.MemoryModule[j].Size;
          if t=0 then begin
            for j:=0 to Machine.SMBIOS.MemoryDeviceCount-1 do
              t:=t+Machine.SMBIOS.MemoryDevice[j].Size;
          end;
          if t=0 then
            t:=Memory.PhysicalTotal shr 20;
          SubItems.Add(IntToStr(t));
          t:=0;
          c:=0;
          cn:=0;
          dn:=0;
          tn:=0;
          for j:=0 to Storage.PhysicalCount-1 do begin
            if (Storage.Physical[j].MediaType=FixedMedia) and (Storage.Physical[j].DeviceType=FILE_DEVICE_DISK) then begin
              Inc(c);
              t:=t+Storage.Physical[j].Size shr 30;
            end;
            if (Storage.Physical[i].BusType in [BusTypeScsi..BusTypeSata]) then begin
              if Storage.Physical[j].DeviceType=FILE_DEVICE_CD_ROM then
                Inc(cn);
              if Storage.Physical[j].DeviceType=FILE_DEVICE_DVD then
                Inc(dn);
              if Storage.Physical[j].DeviceType=FILE_DEVICE_TAPE then
                Inc(tn);
            end;
          end;
          SubItems.Add(IntToStr(c));
          SubItems.Add(IntToStr(t));
          SubItems.Add(IntToStr(cn));
          SubItems.Add(IntToStr(dn));
          SubItems.Add(IntToStr(tn));
          SubItems.Add(Trim(Format('%s %s',[Trim(Machine.SMBIOS.SystemManufacturer),
                                     Trim(Machine.SMBIOS.SystemModel)])));
          SubItems.Add(Trim(Format('%s %s',[Trim(Machine.SMBIOS.MainBoardManufacturer),
                                     Trim(Machine.SMBIOS.MainBoardModel)])));

          if Display.AdapterCount>0 then
            SubItems.Add(Display.Adapter[0].Name)
          else
            SubItems.Add('');

          osn:=Format('%s %d.%d.%d %s',[OS.OSName,
                                        OS.MajorVersion,
                                        OS.Minorversion,
                                        OS.BuildNumber,
                                        OS.OSEdition]);
          SubItems.Add(osn);
          SubItems.Add(OS.MachineGUID);
          SubItems.Add(OS.ProductKey);
        end;

        with SWList, SIC do begin
          for j:=0 to Software.Count-1 do begin
            l:=FindCaption(0,Software.InstallEntry[j].Name,False,True,True);
            if Assigned(l) then begin
              l.SubItems[0]:=IntToStr(StrToInt(l.SubItems[0])+1);
              l.SubItems[1]:=l.SubItems[1]+','+Machine.MachineName;
            end else
              with Items.Add do begin
                Caption:=Software.InstallEntry[j].Name;
                SubItems.Add('1');
                SubItems.Add(Machine.MachineName);
              end;
          end;
        end;
        with OSList, SIC do begin
          l:=FindCaption(0,osn,False,True,True);
          if Assigned(l) then begin
            l.SubItems[0]:=IntToStr(StrToInt(l.SubItems[0])+1);
            l.SubItems[1]:=l.SubItems[1]+','+Machine.MachineName;
          end else
            with Items.Add do begin
              Caption:=osn;
              SubItems.Add('1');
              SubItems.Add(Machine.MachineName);
            end;
        end;
        with NetList.Items.Add, SIC do begin
          Caption:=Machine.MachineName;
          s1:='';
          s2:='';
          for j:=0 to Network.TCPIP.AdapterCount-1 do 
            if Pos('0.0.0.0',Network.TCPIP.Adapter[j].IPAddress.CommaText)=0 then begin
              s1:=s1+Network.TCPIP.Adapter[j].IPAddress.CommaText+',';
              s2:=s2+Network.TCPIP.Adapter[j].Address+',';
            end;
          SetLength(s1,Length(s1)-1);
          SetLength(s2,Length(s2)-1);
          SubItems.Add(s1);
          SubItems.Add(s2);
        end;
      end;
    end;
  end;
end;

end.
