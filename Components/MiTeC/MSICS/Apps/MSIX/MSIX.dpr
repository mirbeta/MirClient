{$INCLUDE ..\..\Compilers.Inc}

{$IFDEF VER130}
This application does not work under Delphi 5.
It can be compiled but in runtime throws errors from KERNELBASE.DLL.
I'm afraid but i don't know why.
if you want to play with it, remove this block.
{$ENDIF}

program MSIX;

uses
  Forms,
  Windows,
  MiTeC_IPC,
  MiTeC_Routines,
  Main in 'Main.pas' {wnd_msi_Main},
  Viewer in 'Viewer.pas' {mdi_msi_Viewer},
  MSI_Splash in 'MSI_Splash.pas' {scrMSI_Splash},
  Summary in 'Summary.pas' {wnd_msi_Summary},
  EODlg in 'EODlg.pas' {dlg_msi_EO},
  ResourcesDlg in 'ResourcesDlg.pas' {dlg_msi_Resources},
  DetailDlg in 'DetailDlg.pas' {dlg_msi_Detail},
  OverviewDlg in 'OverviewDlg.pas' {dlg_msi_Overview},
  PrefsDlg in 'PrefsDlg.pas' {dlg_msi_Prefs},
  UserPictureWnd in 'UserPictureWnd.pas' {wnd_msi_UserPicture},
  Codecs in 'Codecs.pas',
  LngResources in 'LngResources.pas';

{$R *.res}
{$SetPEFlags IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP or IMAGE_FILE_NET_RUN_FROM_SWAP}
{$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE}
{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

const
  AppID = '{08C3116D-C83B-4241-B939-277A3A6D3E2D}';

begin
  Application.Initialize;

  if InstanceExists(AppId,Twnd_msi_Main.Classname,True,True) then
    Exit;

  Application.UpdateFormatSettings:=True;
  FixLocale;
  Application.UpdateFormatSettings:=False;

  Application.Title := 'MiTeC System Information X';
  Application.CreateForm(Twnd_msi_Main, wnd_msi_Main);
  Application.Run;
end.


