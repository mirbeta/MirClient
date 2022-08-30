{*******************************************************************}
{ TWEBUPDATE Wizard component                                       }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 1998-2014                                          }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit WUpdateWiz;

interface

uses
  WUpdate, WuWizForm, Classes, Windows, SysUtils, Forms, Controls,
  Graphics, StdCtrls, Dialogs;

type
  TWebUpdateWizardLanguage = class(TComponent)
  private
    FStartButton: string;
    FWelcome: string;
    FGetUpdateButton: string;
    FRestartButton: string;
    FNewVersion: string;
    FNextButton: string;
    FNewVersionFound: string;
    FExitButton: string;
    FCurrentVersion: string;
    FNoNewVersionAvail: string;
    FNoFilesFound: string;
    FCannotConnect: string;
    FNoUpdateOnServer: string;
    FNewVersionAvail: string;
    FWhatsNew: string;
    FComponentsAvail: string;
    FRestartInfo: string;
    FDownLoadingFiles: string;
    FCurrentProgress: string;
    FNotAcceptLicense: string;
    FTotalProgress: string;
    FUpdateComplete: string;
    FLicense: string;
    FAcceptLicense: string;
    FCancelButton: string;
    FFailedDownload: string;
    FWhatsNewPopup: string;
    FLicensePopup: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Welcome: string read FWelcome write FWelcome;
    property StartButton: string read FStartButton write FStartButton;
    property NextButton: string read FNextButton write FNextButton;
    property ExitButton: string read FExitButton write FExitButton;
    property RestartButton: string read FRestartButton write FRestartButton;
    property CancelButton: string read FCancelButton write FCancelButton;
    property FailedDownload: string read FFailedDownload write FFailedDownload;
    property GetUpdateButton: string read FGetUpdateButton write FGetUpdateButton;
    property NewVersionFound: string read FNewVersionFound write FNewVersionFound;
    property NewVersion: string read FNewVersion write FNewVersion;
    property NoNewVersionAvail: string read FNoNewVersionAvail write FNoNewVersionAvail;
    property NewVersionAvail: string read FNewVersionAvail write FNewVersionAvail;
    property CurrentVersion: string read FCurrentVersion write FCurrentVersion;
    property NoFilesFound: string read FNoFilesFound write FNoFilesFound;
    property NoUpdateOnServer: string read FNoUpdateOnServer write FNoUpdateOnServer;
    property CannotConnect: string read FCannotConnect write FCannotConnect;
    property WhatsNew: string read FWhatsNew write FWhatsNew;
    property License: string read FLicense write FLicense;
    property AcceptLicense: string read FAcceptLicense write FAcceptLicense;
    property NotAcceptLicense: string read FNotAcceptLicense write FNotAcceptLicense;
    property ComponentsAvail: string read FComponentsAvail write FComponentsAvail;
    property DownloadingFiles: string read FDownLoadingFiles write FDownloadingFiles;
    property CurrentProgress: string read FCurrentProgress write FCurrentProgress;
    property TotalProgress: string read FTotalProgress write FTotalProgress;
    property UpdateComplete: string read FUpdateComplete write FUpdateComplete;
    property RestartInfo: string read FRestartInfo write FRestartInfo;
    property WhatsNewPopup: string read FWhatsNewPopup write FWhatsNewPopup;
    property LicensePopup: string read FLicensePopup write FLicensePopup;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizard = class(TComponent)
  private
    FWuWiz: TWuWiz;
    FWebUpdate: TWebUpdate;
    FCaption: string;
    FPosition: TPosition;
    FBorderStyle: TFormBorderStyle;
    FAutoRun: Boolean;
    FAutoStart: Boolean;
    FBillBoard: TBitmap;
    FFont: TFont;
    FWizardLanguage: TWebUpdateWizardLanguage;
    FBillBoardStretch: Boolean;
    FBillBoardCenter: Boolean;
    FBillBoardTop: integer;
    FBillBoardLeft: integer;
    FBillBoardHeight: integer;
    FBillBoardWidth: integer;
    procedure SetBillboard(const Value: TBitmap);
    procedure SetFont(const Value: TFont);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(InitPath: boolean = true);
    procedure InitLanguage(AWizard: TWuWiz; ALanguage: TWebUpdateWizardLanguage);
    procedure CloseWizard;
  published
    property AutoStart: Boolean read FAutoStart write FAutoStart default False;
    property AutoRun: Boolean read FAutoRun write FAutoRun default False;
    property BillBoard: TBitmap read FBillBoard write SetBillboard;
    property BillBoardStretch: Boolean read FBillBoardStretch write FBillBoardStretch default false;
    property BillBoardCenter: Boolean read FBillBoardCenter write FBillBoardCenter default false;
    property BillBoardTop: integer read FBillBoardTop write FBillBoardTop default 27;
    property BillBoardLeft: integer read FBillBoardLeft write FBillBoardLeft default 9;
    property BillBoardHeight: integer read FBillBoardHeight write FBillBoardHeight default 201;
    property BillBoardWidth: integer read FBillBoardWidth write FBillBoardWidth default 116;

    property Caption: string read FCaption write FCaption;
    property Font: TFont read FFont write SetFont;
    property Language: TWebUpdateWizardLanguage read FWizardLanguage write FWizardLanguage;
    property Position: TPosition read FPosition write FPosition default poScreenCenter;
    property WebUpdate: TWebUpdate read FWebUpdate write FWebUpdate;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog;
  end;


implementation

{ TWebUpdateWizard }

procedure TWebUpdateWizard.CloseWizard;
begin
  FWuWiz.Close;
end;

constructor TWebUpdateWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WebUpdate := nil;
  BorderStyle := bsDialog;
  FBillboard := TBitmap.Create;
  FBillBoardCenter := false;
  FBillBoardStretch := false;
  FBillBoardTop := 27;
  FBillBoardLeft := 9;
  FBillBoardWidth := 116;
  FBillBoardHeight := 201;
  FFont := TFont.Create;
  FFont.Name := 'Verdana';
  FFont.Style := [fsBold];
  FFont.Size := 9;
  FWizardLanguage := nil;
  FPosition := poScreenCenter;
end;

destructor TWebUpdateWizard.Destroy;
begin
  FBillboard.Free;
  FFont.Free;
  inherited;
end;

procedure TWebUpdateWizard.Execute(InitPath: boolean = true);
var
  WPC: TWebUpdateProgressCancel;
  WFP: TWebUpdateFileProgress;

begin
  if not Assigned(WebUpdate) then
    raise Exception.Create('No WebUpdate component assigned');


  if InitPath then
    SetCurrentDir(ExtractFilePath(Application.EXEName));


  FWuWiz := TWuWiz.Create(Self);
  FWuWiz.BorderStyle := FBorderStyle;

  if not BillBoard.Empty then
    FWuWiz.Billboard.Picture.Assign(BillBoard);

  FWuWiz.Billboard.Top := FBillBoardTop;
  FWuWiz.Billboard.Left := FBillBoardLeft;
  FWuWiz.Billboard.Height := FBillBoardHeight;
  FWuWiz.Billboard.Width := FBillBoardWidth;

  FWuWiz.BillBoard.Stretch := FBillBoardStretch;
  FWuWiz.BillBoard.Center := FBillBoardCenter;

  FWuWiz.Font.Assign(FFont);

  InitLanguage(FWuWiz, Language);

  if FBorderStyle = bsNone then
  begin
    FWuWiz.Height := FWuWiz.Shape1.Height + 2;
    FWuWiz.Width := FWuWiz.Shape1.Width + 2;
  end
  else
  begin
    FWuWiz.Shape1.Pen.Color := clBtnFace;
    FWuWiz.Shape1.Pen.Width := 0;
  end;

  FWuWiz.AutoRun := FAutoRun;
  FWuWiz.AutoStart := FAutoStart;

  try
    WPC := nil;
    WFP := nil;
    if Assigned(WebUpdate) then
    begin
      WPC := WebUpdate.OnProgressCancel;
      WFP := WebUpdate.OnFileProgress;
    end;

    FWuWiz.WebUpdate := WebUpdate;
    // traps the OnProgressCancel & OnFileProgress events
    FWuWiz.Caption := FCaption;
    FWuWiz.Position := FPosition;
    FWuWiz.ShowModal;

    if Assigned(WebUpdate) then
    begin
      WebUpdate.OnProgressCancel := WPC;
      WebUpdate.OnFileProgress := WFP;
    end;

  finally
    FWuWiz.Free;
  end;

end;

procedure TWebUpdateWizard.InitLanguage(AWizard: TWuWiz; ALanguage: TWebUpdateWizardLanguage);
var
  NewLang: TWebUpdateWizardLanguage;
begin
  NewLang := nil;
  if not Assigned(ALanguage) then
  begin
    NewLang := TWebUpdateWizardLanguage.Create(self);
    ALanguage := NewLang;
  end;

  AWizard.WelcomeLabel.Caption := ALanguage.Welcome;
  AWizard.StartButton.Caption := ALanguage.StartButton;
  AWizard.ControlButton.Caption := ALanguage.GetUpdateButton;
  AWizard.Label1.Caption := ALanguage.WhatsNew;
  AWizard.NewButton.Caption := ALanguage.NextButton;
  AWizard.Label2.Caption := ALanguage.License;
  AWizard.RAccept.Caption := ALanguage.AcceptLicense;
  AWizard.RNoAccept.Caption := ALanguage.NotAcceptLicense;
  AWizard.EULAButton.Caption := ALanguage.NextButton;
  AWizard.Label3.Caption := ALanguage.ComponentsAvail;
  AWizard.FilesButton.Caption := ALanguage.NextButton;
  AWizard.Label4.Caption := ALanguage.DownloadingFiles;
  AWizard.Label5.Caption := ALanguage.CurrentProgress;
  AWizard.Label6.Caption := ALanguage.TotalProgress;
  AWizard.CancelButton.Caption := ALanguage.CancelButton;
  AWizard.Label7.Caption := ALanguage.UpdateComplete;
  AWizard.Label8.Caption := ALanguage.RestartInfo;
  AWizard.RestartButton.Caption := ALanguage.RestartButton;

  AWizard.StrNewFound := ALanguage.NewVersionFound;
  AWizard.StrNewVersion := ALanguage.NewVersion;
  AWizard.StrCurVersion := ALanguage.CurrentVersion;
  AWizard.StrNoNewVersion := ALanguage.NoNewVersionAvail;
  AWizard.StrUCNewVersion := ALanguage.NewVersionAvail;
  AWizard.StrGetUpdate := ALanguage.GetUpdateButton;
  AWizard.StrExit := ALanguage.ExitButton;
  AWizard.StrNoNewFiles := ALanguage.NoFilesFound;
  AWizard.StrCannotConnect := ALanguage.CannotConnect;
  AWizard.StrNoUpdate := ALanguage.NoUpdateOnServer;
  AWizard.StrNext := ALanguage.NextButton;
  AWizard.StrFailedDownload := ALanguage.FailedDownload;
  AWizard.StrWhatsNewPopup := ALanguage.WhatsNewPopup;
  AWizard.StrLicensePopup := ALanguage.LicensePopup;

  if Assigned(NewLang) then
    NewLang.Free;
end;

procedure TWebUpdateWizard.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = WebUpdate) then
    WebUpdate := nil;

  if (AOperation = opRemove) and (AComponent = Language) then
    Language := nil;
    
  inherited;
end;

procedure TWebUpdateWizard.SetBillboard(const Value: TBitmap);
begin
  FBillBoard.Assign(Value);
end;

procedure TWebUpdateWizard.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ TWebUpdateWizardLanguage }

procedure TWebUpdateWizardLanguage.Assign(Source: TPersistent);
begin
  if (Source is TWebUpdateWizardLanguage) then
  begin
    FWelcome := (Source as TWebUpdateWizardLanguage).Welcome;
    FStartButton := (Source as TWebUpdateWizardLanguage).StartButton;
    FNextButton := (Source as TWebUpdateWizardLanguage).NextButton;
    FExitButton := (Source as TWebUpdateWizardLanguage).ExitButton;
    FRestartButton := (Source as TWebUpdateWizardLanguage).RestartButton;
    FGetUpdateButton := (Source as TWebUpdateWizardLanguage).GetUpdateButton;
    FNewVersionFound := (Source as TWebUpdateWizardLanguage).NewVersionFound;
    FNewVersion := (Source as TWebUpdateWizardLanguage).NewVersion;
    FNoNewVersionAvail := (Source as TWebUpdateWizardLanguage).NoNewVersionAvail;
    FNewVersionAvail := (Source as TWebUpdateWizardLanguage).NewVersionAvail;
    FCurrentVersion := (Source as TWebUpdateWizardLanguage).CurrentVersion;
    FNoFilesFound := (Source as TWebUpdateWizardLanguage).NoFilesFound;
    FNoUpdateOnServer := (Source as TWebUpdateWizardLanguage).NoUpdateOnServer;
    FCannotConnect := (Source as TWebUpdateWizardLanguage).CannotConnect;
    FWhatsNew := (Source as TWebUpdateWizardLanguage).WhatsNew;
    FLicense := (Source as TWebUpdateWizardLanguage).License;
    FAcceptLicense := (Source as TWebUpdateWizardLanguage).AcceptLicense;
    FNotAcceptLicense := (Source as TWebUpdateWizardLanguage).NotAcceptLicense;
    FComponentsAvail := (Source as TWebUpdateWizardLanguage).ComponentsAvail;
    FDownloadingFiles := (Source as TWebUpdateWizardLanguage).DownLoadingFiles;
    FCurrentProgress := (Source as TWebUpdateWizardLanguage).CurrentProgress;
    FTotalProgress := (Source as TWebUpdateWizardLanguage).TotalProgress;
    FUpdateComplete := (Source as TWebUpdateWizardLanguage).UpdateComplete;
    FRestartInfo := (Source as TWebUpdateWizardLanguage).RestartInfo;
    FCancelButton := (Source as TWebUpdateWizardLanguage).CancelButton;
    FFailedDownload := (Source as TWebUpdateWizardLanguage).FailedDownload;
    FWhatsNewPopup := (Source as TWebUpdateWizardLanguage).WhatsNewPopup;
    FLicensePopup := (Source as TWebUpdateWizardLanguage).LicensePopup;
  end;
end;

constructor TWebUpdateWizardLanguage.Create(AOwner: TComponent);
begin
  inherited;
  FWelcome := 'Press start to start checking for available application updates ...';
  FStartButton := 'Start';
  FNextButton := 'Next';
  FExitButton := 'Exit';
  FCancelButton := 'Cancel';
  FRestartButton := 'Restart';
  FGetUpdateButton := 'Get update';
  FNewVersionFound := 'New version found';
  FNewVersion := 'New version';
  FNoNewVersionAvail := 'No new version available.';
  FNewVersionAvail := 'New version available.';
  FCurrentVersion := 'Current version';
  FNoFilesFound := 'No files found for update';
  FNoUpdateOnServer := 'No update found on server ...';
  FCannotConnect := 'Could not connect to update server or';
  FWhatsNew := 'What''s new';
  FLicense := 'License agreement';
  FAcceptLicense := 'I accept';
  FNotAcceptLicense := 'I do not accept';
  FComponentsAvail := 'Available application components';
  FDownloadingFiles := 'Downloading files';
  FCurrentProgress := 'Current file progress';
  FTotalProgress := 'Total file progress';
  FUpdateComplete := 'Update completed ...';
  FRestartInfo := 'Press restart to start the updated application.';
  FFailedDownload := 'Failed to download updates';
  FWhatsNewPopup := 'View in Notepad';
  FLicensePopup := 'View in Notepad';
end;

end.
