{$INCLUDE ..\..\Compilers.inc}

unit PrefsDlg;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.ComCtrls;
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, ImgList, ComCtrls, ExtCtrls, StdCtrls;
     {$ENDIF}

type
  TPrefs = record
    Updates,
    ActiveDirectory,
    EventLog: Boolean;
  end;

  Tdlg_msi_Prefs = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    GroupBox1: TGroupBox;
    cbxEL: TCheckBox;
    cbxAD: TCheckBox;
    DsGroupBox2: TGroupBox;
    RFTList: TListView;
    cbxWU: TCheckBox;
  private
  public
  end;

function DisplayPrefsDlg(var APrefs: TPrefs): Boolean;

var
  dlg_msi_Prefs: Tdlg_msi_Prefs;

implementation

uses MiTeC_Shell;

function DisplayPrefsDlg;
var
  i: Integer;
  s: string;
begin
  with Tdlg_msi_Prefs.Create(Application.MainForm) do
    try
      with RFTList.Items.Add do begin
        Caption:='SIF';
        SubItems.Add('System Information File');
      end;
      with RFTList.Items.Add do begin
        Caption:='SIS';
        SubItems.Add('Old System Information File');
      end;
      for i:=0 to RFTList.Items.Count-1 do begin
        s:=GetShellExtension('.'+RFTList.Items[i].Caption,'System Information File');
        RFTList.Items[i].Checked:=SameText(s,Application.ExeName);
      end;
      cbxWU.Checked:=APrefs.Updates;
      cbxAD.Checked:=APrefs.ActiveDirectory;
      cbxEL.Checked:=APrefs.EventLog;
      Result:=ShowModal=mrOK;
      if Result then begin
        APrefs.ActiveDirectory:=cbxAD.Checked;
        APrefs.EventLog:=cbxEL.Checked;
        APrefs.Updates:=cbxWU.Checked;

        for i:=0 to RFTList.Items.Count-1 do
          if RFTList.Items[i].Checked then begin
            s:=GetShellExtension('.'+RFTList.Items[i].Caption,'System Information File');
            if not SameText(s,Application.ExeName) then
              RegisterFileType('.'+RFTList.Items[i].Caption,'',RFTList.Items[i].SubItems[0],Application.ExeName+',1',Application.ExeName);
          end else begin
            if SameText(GetShellExtension('.'+RFTList.Items[i].Caption,'System Information File'),Application.ExeName) then
              UnRegisterFileType('.'+RFTList.Items[i].Caption,'System Information File');
          end;
      end;
    finally
      Free;
    end;
end;

{$R *.dfm}

end.
