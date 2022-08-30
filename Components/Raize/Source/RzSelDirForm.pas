{===============================================================================
  RzSelDirForm Unit

  Raize Components - Form Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Forms
  ------------------------------------------------------------------------------
  TRzSelDirForm
    Form file used by TRzSelDirDialog component.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * No changes.
===============================================================================}

{$I RzComps.inc}
{$WARN SYMBOL_DEPRECATED OFF}

unit RzSelDirForm;

interface

uses
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  FileCtrl,
  Buttons,
  ExtCtrls,
  RzFilSys,
  Mask,
  RzEdit,
  ComCtrls,
  RzTreeVw,
  RzPanel,
  RzDlgBtn;

type
  TRzSelDirForm = class(TForm)
    PnlFolders: TRzPanel;
    PnlPrompt: TRzPanel;
    PnlDrives: TRzPanel;
    LblDrives: TLabel;
    LblDir: TLabel;
    EdtDir: TRzEdit;
    LblFolders: TLabel;
    LblPrompt: TLabel;
    PnlButtons: TRzDialogButtons;
    procedure FormCreate(Sender: TObject);
    procedure EdtDirEnter(Sender: TObject);
    procedure EdtDirExit(Sender: TObject);
    procedure PnlButtonsClickOk(Sender: TObject);
    procedure PnlButtonsClickHelp(Sender: TObject);
  private
    function GetDirectory: string;
    procedure SetDirectory( Value: string );
    procedure TvwDirsChange(Sender: TObject; Node: TTreeNode);
  public
    TvwDirs: TRzDirectoryTree;
    AllowCreate: Boolean;
    property Directory: string
      read GetDirectory
      write SetDirectory;
  end;

implementation

{$R *.dfm}

resourcestring
  sRzConfirmNewDirectory = 'Please confirm that you wish to create the "%s" directory.';
  sRzCreateDirError      = 'Directory could not be created. Access Denied.';


{===========================}
{== TRzSelDirForm Methods ==}
{===========================}

procedure TRzSelDirForm.FormCreate(Sender: TObject);
begin
  TvwDirs := TRzDirectoryTree.Create( Self );
  with TvwDirs do
  begin
    Parent := PnlFolders;
    Align := alClient;
    DirLabel := LblDir;
    TabOrder := 0;
    OnChange := TvwDirsChange;
  end;

  PnlDrives.Visible := False;

  PnlPrompt.FullRepaint := False;
  PnlFolders.FullRepaint := False;
  PnlDrives.Fullrepaint := False;
end;


function TRzSelDirForm.GetDirectory: string;
begin
  Result := EdtDir.Text;
end;


procedure TRzSelDirForm.SetDirectory( Value: string );
begin
  try
    TvwDirs.Directory := Value;
  except
    TvwDirs.Directory := '';
  end;
end;


procedure TRzSelDirForm.TvwDirsChange(Sender: TObject; Node: TTreeNode);
begin
  EdtDir.Text := TvwDirs.Directory;
  LblDir.Hint := EdtDir.Text;
  EdtDir.Hint := EdtDir.Text;
end;


procedure TRzSelDirForm.EdtDirEnter(Sender: TObject);
begin
  PnlButtons.OkDefault := True;
end;

procedure TRzSelDirForm.EdtDirExit(Sender: TObject);
begin
  PnlButtons.OkDefault := False;
end;

procedure TRzSelDirForm.PnlButtonsClickOk(Sender: TObject);
begin
  if AllowCreate and not DirectoryExists( Directory ) then
  begin
    if MessageDlg( Format( sRzConfirmNewDirectory, [ Directory ] ), mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
    begin
      try
        ForceDirectories( Directory )
      except
        on EInOutError do
        begin
          MessageDlg( sRzCreateDirError, mtError, [ mbOK ], 0 );
          ModalResult := 0;
        end;
      end;

    end
    else
      ModalResult := 0;
  end;
end;

procedure TRzSelDirForm.PnlButtonsClickHelp(Sender: TObject);
begin
  Application.HelpContext( PnlButtons.HelpContext );
end;

end.

