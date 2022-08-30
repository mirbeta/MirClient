{*******************************************************************}
{ TWebUpdate Wizard form                                            }
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

unit WuWizForm;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, WUpdate, ComCtrls, ExtCtrls, CheckLst, Math, ShellAPI;

const
  AUTORUNDELAY = 350;

type
  TWUWIZ = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Billboard: TImage;
    WelcomeLabel: TLabel;
    StartButton: TButton;
    TabSheet3: TTabSheet;
    VersionInfoLabel: TLabel;
    ControlButton: TButton;
    WhatsNewMemo: TMemo;
    Label1: TLabel;
    TabSheet4: TTabSheet;
    Label2: TLabel;
    EULAMemo: TMemo;
    RAccept: TRadioButton;
    RNoAccept: TRadioButton;
    TabSheet5: TTabSheet;
    CheckListBox1: TCheckListBox;
    Label3: TLabel;
    NewButton: TButton;
    EULAButton: TButton;
    TabSheet6: TTabSheet;
    Label4: TLabel;
    FileProgress: TProgressBar;
    TotalProgress: TProgressBar;
    CancelButton: TButton;
    Label5: TLabel;
    Label6: TLabel;
    FilesButton: TButton;
    TabSheet7: TTabSheet;
    RestartButton: TButton;
    Label7: TLabel;
    Label8: TLabel;
    FileLabel: TLabel;
    Shape1: TShape;
    PopupMenu1: TPopupMenu;
    ViewinNotepad1: TMenuItem;
    PopupMenu2: TPopupMenu;
    ViewinNotepad2: TMenuItem;
    EULARichEdit: TRichEdit;
    WhatsNewRichEdit: TRichEdit;
    procedure ViewinNotepad2Click(Sender: TObject);
    procedure ViewinNotepad1Click(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure ControlButtonClick(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure EULAButtonClick(Sender: TObject);
    procedure RAcceptClick(Sender: TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure WebUpdateFileProgress(Sender: TObject; filename: String;
      pos, size: Integer);
    procedure WebUpdateCancel(Sender: TObject; var Cancel: Boolean);  
    procedure FormCreate(Sender: TObject);
    procedure RestartButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    FWebUpdate: TWebUpdate;
    FCancelled: Boolean;
    FAutoRun: Boolean;
    FAutoStart: Boolean;
    FStrNewFound: string;
    FStrNoUpdate: string;
    FStrNoNewFiles: string;
    FStrGetUpdate: string;
    FStrNewVersion: string;
    FStrNext: string;
    FStrNoNewVersion: string;
    FStrUCNewVersion: string;
    FStrCannotConnect: string;
    FStrCurVersion: string;
    FStrExit: string;
    FFailedDownload: string;
    FStrLicensePopup: string;
    FStrWhatsNewPopup: string;
    procedure SetWebUpdate(const Value: TWebUpdate);
    procedure SetCancelled(const Value: Boolean);
    procedure ClickDelay;
    procedure SetButtonWidth(Button: TButton);
    procedure DownloadFiles;
  public
    { Public declarations }
    procedure UpdateDone;
    function CheckFileCount: Boolean;
    property WebUpdate: TWebUpdate read FWebUpdate write SetWebUpdate;
    property Cancelled: Boolean read FCancelled write SetCancelled;
    property AutoRun: Boolean read FAutoRun write FAutoRun;
    property AutoStart: Boolean read FAutoStart write FAutoStart;

    property StrNewFound: string read FStrNewFound write FStrNewFound;
    property StrNewVersion: string read FStrNewVersion write FStrNewVersion;
    property StrCurVersion: string read FStrCurVersion write FStrCurVersion;
    property StrNoNewVersion: string read FStrNoNewVersion write FStrNoNewVersion;
    property StrUCNewVersion: string read FStrUCNewVersion write FStrUCNewVersion;
    property StrGetUpdate: string read FStrGetUpdate write FStrGetUpdate;
    property StrExit: string read FStrExit write FStrExit;
    property StrNoNewFiles: string read FStrNoNewFiles write FStrNoNewFiles;
    property StrCannotConnect: string read FStrCannotConnect write FStrCannotConnect;
    property StrNoUpdate: string read FStrNoUpdate write FStrNoUpdate;
    property StrNext: string read FStrNext write FStrNext;
    property StrFailedDownload:string read FFailedDownload write FFailedDownload;
    property StrWhatsNewPopup: string read FStrWhatsNewPopup write FStrWhatsNewPopup;
    property StrLicensePopup: string read FStrLicensePopup write FStrLicensePopup;
  end;

//var
//  WUWIZ: TWUWIZ;

implementation

{$R *.dfm}

procedure TWUWIZ.SetButtonWidth(Button: TButton);
var
  MyCanvas: TCanvas;
  iButtonRight, iButtonWidth: Integer;
begin
  iButtonRight := Button.Left + Button.Width;
  iButtonWidth := Button.Width; // or 91 if it shall be able to shrink again
  MyCanvas := TCanvas.Create;
  try
    MyCanvas.Handle := GetDC(Button.Handle);
    MyCanvas.Font.Assign(Button.Font);
    Button.Width := Max(MyCanvas.TextWidth(Button.Caption) + 16, iButtonWidth);
  finally
    ReleaseDC(0, MyCanvas.Handle);
    FreeAndNil(MyCanvas);
  end;
  //Button.Left := iButtonLeft - (ControlButton.Width - iButtonWidth);
  Button.Left := iButtonRight - Button.Width;
end;

procedure TWUWIZ.StartButtonClick(Sender: TObject);
var
  res: Integer;
  cv: string;
begin
  Cursor := crHourGlass;
  StartButton.Enabled := false;
  ControlButton.Enabled := false;

  ViewInNotepad1.Caption := StrWhatsNewPopup;
  ViewInNotepad2.Caption := StrLicensePopup;

  if (WebUpdate.StartConnection = WU_SUCCESS) then
  begin
    Cursor := crDefault;
    if WebUpdate.UpdateType = ftpUpdate then
      WebUpdate.FTPConnect;

    WebUpdate.UpdateUpdate := wuuSilent;

    if (WebUpdate.GetControlFile = WU_SUCCESS) then
    begin
      StartButton.Enabled := true;
      PageControl1.ActivePage := TabSheet2;
      ControlButton.Enabled := true;

      res := WebUpdate.DoVersionCheck;
      case res of
      WU_DATEBASEDNEWVERSION:
        begin
          if WebUpdate.CurVersionDate <> EncodeDate(1980,1,1) then
            cv := StrCurVersion + ' : ' + DateToStr(WebUpdate.CurVersionDate);

        if (Frac(WebUpdate.NewVersionDate) <> 0) then
          VersionInfoLabel.Caption := StrNewFound + ' :' + #13 +
            cv + #13 +
            StrNewVersion + ' : ' + DateToStr(WebUpdate.NewVersionDate) + ' ' + TimeToStr(WebUpdate.NewVersionDate) + #13#13+
            WebUpdate.UpdateDescription
        else
          VersionInfoLabel.Caption := StrNewFound + ' :' + #13 +
            cv + #13 +
            StrNewVersion + ' : ' + DateToStr(WebUpdate.NewVersionDate) + #13#13+
            WebUpdate.UpdateDescription;
        end;
      WU_UNCONDITIONALNEWVERSION,WU_CHECKSUMBASEDNEWVERSION,WU_FILESIZEBASEDNEWVERSION, WU_CUSTOMNEWVERSION:
        VersionInfoLabel.Caption := StrUcNewVersion;
      WU_VERSIONINFOBASEDNEWVERSION:
        VersionInfoLabel.Caption := StrNewFound + ' :' + #13 +
          StrCurVersion + ' : '+ WebUpdate.CurVersionInfo + #13 +
          StrNewVersion + ' : '+ WebUpdate.NewVersionInfo+ #13#13+
          WebUpdate.UpdateDescription;
      WU_NONEWVERSION:
        VersionInfoLabel.Caption := StrNoNewVersion;
      end;

      VersionInfoLabel.Width := 300;

      if res <> WU_NONEWVERSION then
        ControlButton.Caption := StrGetUpdate
      else
        ControlButton.Caption := StrExit;

      SetButtonWidth(ControlButton);

      ControlButton.Enabled := True;
      ControlButton.SetFocus;

      if AutoRun then
      begin
        ClickDelay;
        ControlButtonClick(self);
      end;
    end
    else
    begin
      PageControl1.ActivePage := TabSheet2;

      VersionInfoLabel.Caption := StrCannotConnect + #13 + StrNoUpdate;

      VersionInfoLabel.Width := 300;

      ControlButton.Caption := StrExit;

      SetButtonWidth(ControlButton);

      ControlButton.Enabled := True;
      ControlButton.SetFocus;
      
      if AutoRun then
      begin
        ClickDelay;
        ControlButtonClick(self);
      end;
    end;
  end
  else
  begin
    Cursor := crDefault;
    UpdateDone;
    StartButton.Enabled := true;
  end;
end;

procedure TWUWIZ.ControlButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  sl: TStringList;
  i,j: Integer;
  res: Integer;
  s: ansistring;
  isRTF: boolean;
  descr: string;
begin
  ControlButton.Enabled := false;
  if ControlButton.Caption <> StrExit then
  begin
    // check for custom actions to handle
    if WebUpdate.HandleActions = WU_SUCCESS then
    begin
      // check for a What's new file
      ms := WebUpdate.GetWhatsNewStream;

      if Assigned(ms) and (ms.Size > 0) then
      begin
        res := mrOK;

        SetLength(s,10);
        ms.Position := 0;
        ms.Read(s[1], 5);
        ms.Position := 0;
        isRTF := true;
        if pos(ansistring('{\RTF'),Uppercase(string(s))) = 0 then
        begin
          isRTF := false;
          sl := TStringList.Create;
          sl.LoadFromStream(ms);

          if Assigned(WebUpdate.OnDownloadedWhatsNew) then
            WebUpdate.OnDownloadedWhatsNew(WebUpdate, sl, res);

          ms.Clear;
          sl.SaveToStream(ms);
          ms.Position := 0;
          sl.Free;
        end;

        if res = mrOk then
        begin
          WhatsNewRichEdit.Visible := isRTF;
          WhatsNewMemo.Visible := not isRTF;

          if isRTF then
            WhatsNewRichEdit.Lines.LoadFromStream(ms)
          else
            WhatsNewMemo.Lines.LoadFromStream(ms);

          ms.Free;

          PageControl1.ActivePage := TabSheet3;
          NewButton.Enabled := true;
          NewButton.SetFocus;

          if AutoRun then
          begin
            ClickDelay;
            NewButtonClick(self);
          end;
          Exit;
        end;
      end;

      // check for a EULA file
      ms := WebUpdate.GetEULAStream;

      if Assigned(ms) and (ms.Size > 0) then
      begin
        res := mrRetry;

        if res = mrAbort then
        begin
          UpdateDone;
          Exit;
        end;

        if (res = mrRetry) then
        begin
          SetLength(s,10);
          ms.Position := 0;
          ms.Read(s[1],5);
          ms.Position := 0;

          if pos(ansistring('{\RTF'),UpperCase(string(s))) = 1 then
          begin
            EULAMemo.Visible := false;
            EULARichEdit.Visible := true;
            EULARichEdit.Lines.LoadFromStream(ms);
          end
          else
          begin
            sl := TStringList.Create;
            sl.LoadFromStream(ms);

            if Assigned(WebUpdate.OnDownloadedEULA) then
              WebUpdate.OnDownloadedEULA(WebUpdate, sl, res);

            ms.Clear;
            sl.SaveToStream(ms);
            sl.Free;
            ms.Position := 0;
            EULAMemo.Lines.LoadFromStream(ms);
          end;

          ms.Free;

          PageControl1.ActivePage := TabSheet4;

          if AutoRun then
          begin
            EULAButton.Enabled := true;
            EULAButton.Caption := StrNext;

            SetButtonWidth(EULAButton);

            ClickDelay;
            EULAButtonClick(self);
          end;
          Exit;
        end;
      end;

      // Get list of file details
      WebUpdate.GetFileDetails;
      WebUpdate.ProcessFileDetails;
      
      for i := 1 to WebUpdate.FileList.Count do
      begin

        if not WebUpdate.FileList.Items[i - 1].Hidden then
        begin
          descr := WebUpdate.FileList.Items[i - 1].Description;
          if descr = '' then
            descr := 'Application';
          CheckListBox1.Items.Add(descr);
        end;
      end;

      j := 0;
      for i := 1 to CheckListBox1.Items.Count do
      begin
        if not WebUpdate.FileList.Items[i - 1].Hidden then
        begin
          CheckListBox1.Checked[j] := True;
          CheckListBox1.Items.Objects[j] := TObject(i - 1);
          if WebUpdate.FileList.Items[i - 1].Mandatory then
            CheckListBox1.ItemEnabled[j] := false;
          inc(j);
        end;
      end;

      if CheckFileCount then
      begin

        if CheckListBox1.Items.Count = 0 then
        begin
          DownloadFiles;
          Exit;
        end
        else
        begin
          PageControl1.ActivePage := TabSheet5;
          FilesButton.Enabled := true;
          FilesButton.SetFocus;

          if AutoRun or (CheckListBox1.Items.Count = 0) then
          begin
            ClickDelay;
            FilesButtonClick(Self);
          end;
        end;
      end
      else
      begin
        PageControl1.ActivePage := TabSheet7;
        RestartButton.Caption := StrExit;
        SetButtonWidth(RestartButton);
        Label8.Caption := '';
        RestartButton.SetFocus;
      end;
    end
    else
      UpdateDone;
  end
  else
    UpdateDone;
end;

procedure TWUWIZ.NewButtonClick(Sender: TObject);
var
  sl: TMemoryStream;
  i,j: Integer;
  s: ansistring;
begin
  sl := WebUpdate.GetEULAStream;

  if Assigned(sl) and (sl.Size > 0) then
  begin
    sl.Position := 0;
    SetLength(s,10);
    sl.Read(s[1], 5);
    sl.Position := 0;

    if pos(ansistring('{\RTF'),UpperCase(string(s))) = 1 then
    begin
      EULAMemo.Visible := false;
      EULARichEdit.Visible := true;
      EULARichEdit.Lines.LoadFromStream(sl);
    end
    else
      EULAMemo.Lines.LoadFromStream(sl);

    sl.Free;

    PageControl1.ActivePage := TabSheet4;

    if AutoRun then
    begin
      EULAButton.Enabled := true;
      EULAButton.Caption := StrNext;
      SetButtonWidth(EULAButton);
      ClickDelay;
      EULAButtonClick(Self);
    end;
    Exit;
  end;

  WebUpdate.GetFileDetails;
  WebUpdate.ProcessFileDetails;

  for i := 1 to WebUpdate.FileList.Count do
  begin
    if not WebUpdate.FileList.Items[i - 1].Hidden then
      CheckListBox1.Items.Add(WebUpdate.FileList.Items[i - 1].Description);
  end;

  j := 0;
  for i := 1 to WebUpdate.FileList.Count do
  begin
    if not WebUpdate.FileList.Items[i - 1].Hidden then
    begin
      CheckListBox1.Checked[j] := True;
      CheckListBox1.Items.Objects[j] := TObject(i - 1);
      if WebUpdate.FileList.Items[i - 1].Mandatory then
        CheckListBox1.ItemEnabled[j] := false;
      inc(j);
    end;
  end;

  if CheckFileCount then
  begin
    if (CheckListBox1.Items.Count = 0) then
    begin
      DownloadFiles;
    end
    else
    begin
      PageControl1.ActivePage := TabSheet5;
      FilesButton.Enabled := true;
      FilesButton.SetFocus;
    end;

    if AutoRun then
    begin
      ClickDelay;
      FilesButtonClick(self);
    end;
  end;
end;

procedure TWUWIZ.EULAButtonClick(Sender: TObject);
var
  i,j: Integer;
begin
  if RAccept.Checked then
  begin
    WebUpdate.GetFileDetails;
    WebUpdate.ProcessFileDetails;

    for i := 1 to WebUpdate.FileList.Count do
    begin
      if not WebUpdate.FileList.Items[i - 1].Hidden then
        CheckListBox1.Items.Add(WebUpdate.FileList.Items[i - 1].Description);
    end;

    j := 0;
    for i := 1 to WebUpdate.FileList.Count do
    begin
      if not WebUpdate.FileList.Items[i - 1].Hidden then
      begin
        CheckListBox1.Checked[j] := WebUpdate.FileList.Items[i - 1].Preselect;
        CheckListBox1.Items.Objects[j] := TObject(i - 1);
        if WebUpdate.FileList.Items[i - 1].Mandatory then
          CheckListBox1.ItemEnabled[j] := false;
        inc(j);
      end;
    end;


    if CheckListBox1.Items.Count = 0 then
    begin
      DownloadFiles;
    end
    else
    begin
      PageControl1.ActivePage := TabSheet5;

      FilesButton.Enabled := true;
      FilesButton.SetFocus;

      if AutoRun then
      begin
        ClickDelay;
        FilesButtonClick(Self);
      end;
    end;
  end;

  if RNoAccept.Checked then
    UpdateDone;
end;

procedure TWUWIZ.RAcceptClick(Sender: TObject);
begin
  if RAccept.Checked then
  begin
    EULAButton.Enabled := True;
    EULAButton.Caption := StrNext;
    SetButtonWidth(EULAButton);
  end;

  if RNoAccept.Checked then
  begin
    EULAButton.Enabled := True;
    EULAButton.Caption := StrExit;
    SetButtonWidth(EULAButton);
  end;
end;

procedure TWUWiz.DownloadFiles;
begin
  FileLabel.Caption := '';
  FileProgress.Position := 0;
  TotalProgress.Position := 0;

  PageControl1.ActivePage := TabSheet6;
  CancelButton.Enabled := true;
  CancelButton.SetFocus;

  if AutoRun then
    CancelButton.Enabled := false;

  Cursor := crHourGlass;

  if WebUpdate.GetFileUpdates = WU_FAILED then
  begin
    Cursor := crDefault;
    WebUpdate.Cancel;
    ShowMessage(FFailedDownload);
  end;

  WebUpdate.UpdateActions;

  Cursor := crDefault;

  if WebUpdate.Cancelled then
  begin
    UpdateDone;
  end
  else
  begin
    WebUpdate.StopConnection;

    if Assigned(WebUpdate.OnSuccess) then
      WebUpdate.OnSuccess(WebUpdate);

    if WebUpdate.AppNeedsRestart then
    begin
      if not WebUpdate.AppSilentRestart then
      begin
        PageControl1.ActivePage := TabSheet7;
        RestartButton.Enabled := true;
        RestartButton.SetFocus;
      end;
    end
    else
    begin
      PageControl1.ActivePage := TabSheet7;
      RestartButton.Caption := StrExit;
      SetButtonWidth(RestartButton);
      Label8.Caption := '';
      RestartButton.Enabled := true;
      RestartButton.SetFocus;
    end;

    if AutoRun or (WebUpdate.AppSilentRestart and WebUpdate.AppNeedsRestart) then
    begin
      if not WebUpdate.AppSilentRestart then
        ClickDelay;

      RestartButtonClick(Self);
    end;
  end;
end;


procedure TWUWIZ.FilesButtonClick(Sender: TObject);
var
  i,j,k: Integer;
begin
  for i := 1 to WebUpdate.FileList.Count do
    WebUpdate.FileList.Items[i - 1].Selected := true;

  // indicate the selected items
  for i := 1 to CheckListBox1.Items.Count do
  begin
    if not CheckListBox1.Checked[i - 1] then
    begin
      k := integer(CheckListBox1.Items.Objects[i - 1]);
      WebUpdate.FileList.Items[k].Selected := false;
    end;
  end;

  j := 0;

  while (j < WebUpdate.FileList.Count) do
  begin
    if not WebUpdate.FileList.Items[j].Selected then
      WebUpdate.FileList.Items[j].Free
    else
      inc(j);
  end;

  if CheckFileCount then
  begin

    DownloadFiles;

  end;
end;

procedure TWUWIZ.WebUpdateFileProgress(Sender: TObject; filename: String;
  pos, size: Integer);
begin
  FileLabel.Caption := ExtractFileName(Filename);
  FileProgress.Max := size;
  FileProgress.Position := pos;

  TotalProgress.Max := WebUpdate.FileList.TotalSize;
  TotalProgress.Position := WebUpdate.FileList.CompletedSize + pos;
  Application.ProcessMessages;
end;

procedure TWUWIZ.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  WebUpdate := nil;
end;

procedure TWUWIZ.RestartButtonClick(Sender: TObject);
begin
  if WebUpdate.AppNeedsRestart then
    WebUpdate.DoRestart
  else
    Close;
end;

procedure TWUWIZ.UpdateDone;
begin
  WebUpdate.StopConnection;
  Close;
end;

procedure TWUWIZ.ViewinNotepad1Click(Sender: TObject);
var
  fname: string;
begin
  fname := WebUpdate.WinTempDir + 'whatsnew.txt';
  WhatsNewMemo.Lines.SaveToFile(fname);
  ShellExecute(0,'open',pchar(fname),nil,nil,SW_NORMAL);
end;

procedure TWUWIZ.ViewinNotepad2Click(Sender: TObject);
var
  fname: string;
begin
  fname := WebUpdate.WinTempDir + 'eula.txt';
  EULAMemo.Lines.SaveToFile(fname);
  ShellExecute(0,'open',pchar(fname),nil,nil,SW_NORMAL);
end;

function TWUWIZ.CheckFileCount: Boolean;
begin
  Result := True;
  if (WebUpdate.FileList.Count = 0) then
  begin
    ShowMessage(StrNoNewFiles);
    UpdateDone;
    Result := False;
  end;
end;

procedure TWUWIZ.SetWebUpdate(const Value: TWebUpdate);
begin
  FWebUpdate := Value;
  if Assigned(FWebUpdate) Then
  begin
    FWebUpdate.OnFileProgress := WebUpdateFileProgress;
    FWebUpdate.OnProgressCancel := WebUpdateCancel;
  end;
  FCancelled := False;
end;

procedure TWUWIZ.SetCancelled(const Value: Boolean);
begin
  FCancelled := Value;
end;

procedure TWUWIZ.WebUpdateCancel(Sender: TObject; var Cancel: Boolean);
begin
  Cancel := FCancelled;
end;

procedure TWUWIZ.CancelButtonClick(Sender: TObject);
begin
  FCancelled := True;
end;

procedure TWUWIZ.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   UpdateDone;
end;

procedure TWUWIZ.FormActivate(Sender: TObject);
begin
  StartButton.SetFocus;
  if AutoStart then
  begin
    PageControl1.ActivePage := TabSheet2;
    ClickDelay;
    StartButtonClick(Self);
  end;
end;

procedure TWUWIZ.ClickDelay;
var
  t: DWord;
begin
  t := GetTickCount;
  while (GetTickCount - t < AUTORUNDELAY) do
    Application.ProcessMessages;
end;

end.
