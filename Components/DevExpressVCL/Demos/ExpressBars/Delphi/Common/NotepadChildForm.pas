unit NotepadChildForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit, RichEdit, dxRibbonForm,
  ExtCtrls;

const
  sDefaultDocName = 'New Document.rtf';

  RTFFilter = 'Rich Text Files (*.RTF)|*.RTF';
  TXTFilter = 'Plain text (*.TXT)|*.TXT';

  UM_UPDATEUNDO = WM_USER + 1;

type
  TNotepadAddToRecentList = procedure (Sender: TObject; const AFileName: string) of object;

  { TRichEditUndoController }

  TRichEditUndoController = class
  private
    FActions: TStringList;
    FEditor: TcxRichEdit;
    FIsLocked: Boolean;
    FLastMessageID: Integer;
  protected
    procedure PopUndo;
    procedure PushUndo(const AAction: string);
  public
    constructor Create(AEditor: TcxRichEdit);
    destructor Destroy; override;
    procedure AddAction(AActionID: Integer);
    procedure AnalyseMessage;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure Redo;
    procedure Undo(ACount: Integer);
    procedure Lock;
    procedure UnLock;

    property Actions: TStringList read FActions;
  end;

  { TfrmNotepadChild }

  TfrmNotepadChild = class(TdxRibbonForm)
    Editor: TcxRichEdit;
    SaveDialog: TSaveDialog;
    bvSpacer4: TBevel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    procedure EditorPropertiesChange(Sender: TObject);
    procedure EditorPropertiesSelectionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FFileName: string;
    FModified: Boolean;
    FUndoController: TRichEditUndoController;

    FOnAddToRecent: TNotepadAddToRecentList;
    FOnChanged: TNotifyEvent;
    FOnUndoListChanged: TNotifyEvent;

    function GetCanEdit: Boolean;
    function GetCanPaste: Boolean;
    function GetCanSave: Boolean;
    function GetLocked: Boolean;
    procedure SetLocked(const AValue: Boolean);
  protected
    procedure DoAddToRecent(const AFileName: string);
    procedure DoChanged;
    procedure DoUndoListChanged;

    function CheckSaveChanges: Boolean;
    function QuerySaveFile: Integer;

    procedure UpdateCaption;
    procedure UpdateUndo(var AMessage: TMessage); message UM_UPDATEUNDO;
  public
    destructor Destroy; override;
    procedure OpenFile(const AFileName: string);
    function ExportAsPlainText: Boolean;
    function SaveFile(ASaveAs: Boolean): Boolean;
    //
    property CanEdit: Boolean read GetCanEdit;
    property CanPaste: Boolean read GetCanPaste;
    property CanSave: Boolean read GetCanSave;
    property FileName: string read FFileName;
    property Locked: Boolean read GetLocked write SetLocked;
    property Modified: Boolean read FModified write FModified;
    property UndoController: TRichEditUndoController read FUndoController;
    //
    property OnAddToRecent: TNotepadAddToRecentList read FOnAddToRecent write FOnAddToRecent;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnUndoListChanged: TNotifyEvent read FOnUndoListChanged write FOnUndoListChanged;
  end;

implementation

uses
  dxCore;

{$R *.dfm}

{ TRichEditUndoController }

constructor TRichEditUndoController.Create(AEditor: TcxRichEdit);
begin
  inherited Create;
  FEditor := AEditor;
  FActions := TStringList.Create;
end;

destructor TRichEditUndoController.Destroy;
begin
  FreeAndNil(FActions);
  inherited Destroy;
end;

procedure TRichEditUndoController.AnalyseMessage;
var
  AMessageID: Integer;
begin
  if FIsLocked then Exit;
  AMessageID := SendMessage(FEditor.InnerControl.Handle, EM_GETUNDONAME, 0, 0);
  if (AMessageID > 1) or (AMessageID = 1) and (AMessageID <> FLastMessageID) then
    AddAction(AMessageID);
end;

function TRichEditUndoController.CanUndo: Boolean;
begin
  Result := (SendMessage(FEditor.InnerControl.Handle, EM_CANUNDO, 0, 0) <> 0) and (Actions.Count > 0);
end;

function TRichEditUndoController.CanRedo: Boolean;
begin
  Result := SendMessage(FEditor.InnerControl.Handle, EM_CANREDO, 0, 0) <> 0;
end;

procedure TRichEditUndoController.Undo(ACount: Integer);
begin
  Lock;
  try
    while ACount > 0 do
    begin
      if CanUndo then
      begin
        PopUndo;
        SendMessage(FEditor.InnerControl.Handle, EM_UNDO, 0, 0);
      end
      else
        Break;
      Dec(ACount);
    end;
  finally
    Unlock;
  end;
end;

procedure TRichEditUndoController.Redo;
begin
  SendMessage(FEditor.InnerControl.Handle, EM_REDO, 0, 0);
end;

procedure TRichEditUndoController.Lock;
begin
  FIsLocked := True;
  FLastMessageID := 0;
end;

procedure TRichEditUndoController.UnLock;
begin
  FIsLocked := False;
end;

procedure TRichEditUndoController.AddAction(AActionID: Integer);
const
  RichEditAction: array[0..6] of string = (
    'Unknown', 'Typing', 'Delete', 'Drag And Drop', 'Cut', 'Paste', 'Color Change'
  );
begin
  if (AActionID <> 6) or (FEditor.SelLength <> 0) then
    PushUndo(RichEditAction[AActionID]);
  FLastMessageID := AActionID;
end;

procedure TRichEditUndoController.PopUndo;
begin
  if Actions.Count > 0 then
    Actions.Delete(0);
end;

procedure TRichEditUndoController.PushUndo(const AAction: string);
begin
  Actions.Insert(0, AAction);
end;

{ TfrmNotepadChild }

destructor TfrmNotepadChild.Destroy;
begin
  FreeAndNil(FUndoController);
  inherited Destroy;
end;

procedure TfrmNotepadChild.OpenFile(const AFileName: string);
begin
  FFileName := AFileName;
  Editor.Lines.LoadFromFile(FileName);
  DoAddToRecent(FileName);
  FModified := False;
  UpdateCaption;
  DoChanged;
end;

function TfrmNotepadChild.ExportAsPlainText: Boolean;
begin
  SaveDialog.FileName := ChangeFileExt(ExtractFileName(FileName), '');
  SaveDialog.Filter := TXTFilter;
  Result := SaveDialog.Execute;
  if Result then
  begin
    Editor.ActiveProperties.PlainText := True;
    Editor.Lines.SaveToFile(SaveDialog.FileName);
    Editor.ActiveProperties.PlainText := False;
  end;
end;

function TfrmNotepadChild.SaveFile(ASaveAs: Boolean): Boolean;
begin
  Result := not ASaveAs and (FileName <> '');
  if not Result then
  begin
    SaveDialog.FileName := ChangeFileExt(ExtractFileName(FileName), '');
    SaveDialog.Filter := RTFFilter;
    Result := SaveDialog.Execute;
    if Result then
    begin
      FFileName := SaveDialog.FileName;
      DoAddToRecent(FileName);
      UpdateCaption;
    end;
  end;

  if Result then
  begin
    Editor.Lines.SaveToFile(FileName);
    FModified := False;
    DoChanged;
  end;
end;

procedure TfrmNotepadChild.DoAddToRecent(const AFileName: string);
begin
  if Assigned(OnAddToRecent) then
    OnAddToRecent(Self, AFileName);
end;

procedure TfrmNotepadChild.DoChanged;
begin
  dxCallNotify(OnChanged, Self);
end;

procedure TfrmNotepadChild.DoUndoListChanged;
begin
  dxCallNotify(OnUndoListChanged, Self);
end;

function TfrmNotepadChild.CheckSaveChanges: Boolean;
begin
  Result := True;
  if Modified then
    case QuerySaveFile of
      ID_YES:
        Result := SaveFile(False);
      ID_CANCEL:
        Result := False;
    end;
end;

function TfrmNotepadChild.QuerySaveFile: Integer;
begin
  Result := Application.MessageBox(
    PChar(Format('Do you want to save the changes you made to "%s"?', [Caption])),
    PChar(Application.Title), MB_ICONQUESTION or MB_YESNOCANCEL);
end;

procedure TfrmNotepadChild.UpdateCaption;
begin
  if FileName <> '' then
    Caption := ExtractFileName(FileName)
  else
    Caption := sDefaultDocName;
end;

procedure TfrmNotepadChild.UpdateUndo(var AMessage: TMessage);
begin
  UndoController.AnalyseMessage;
  DoUndoListChanged;
end;

function TfrmNotepadChild.GetLocked: Boolean;
begin
  Result := Editor.Properties.ReadOnly;
end;

function TfrmNotepadChild.GetCanEdit: Boolean;
begin
  Result := not Locked;
end;

function TfrmNotepadChild.GetCanPaste: Boolean;
begin
  Result := SendMessage(Editor.InnerControl.Handle, EM_CANPASTE, 0, 0) <> 0;
end;

function TfrmNotepadChild.GetCanSave: Boolean;
begin
  Result := Modified or (FileName = '');
end;

procedure TfrmNotepadChild.SetLocked(const AValue: Boolean);
begin
  if Locked <> AValue then
  begin
    Editor.Properties.ReadOnly := AValue;
    DoChanged;
  end;
end;

procedure TfrmNotepadChild.EditorPropertiesChange(Sender: TObject);
begin
  Modified := True;
  DoChanged;
  PostMessage(Handle, UM_UPDATEUNDO, 0, 0);
end;

procedure TfrmNotepadChild.EditorPropertiesSelectionChange(Sender: TObject);
begin
  DoChanged;
end;

procedure TfrmNotepadChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree
end;

procedure TfrmNotepadChild.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSaveChanges;
end;

procedure TfrmNotepadChild.FormCreate(Sender: TObject);
begin
  FUndoController := TRichEditUndoController.Create(Editor);
  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName);
  UpdateCaption;
end;

end.

