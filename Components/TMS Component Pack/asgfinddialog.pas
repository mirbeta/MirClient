{***************************************************************************}
{ TAdvStringGrid Find dialog component                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AsgFindDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, AdvGrid, Types, Grids
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TAdvGridFindDialog = class;
  
  TAsgFindDlg = class(TForm)
    Options: TGroupBox;
    Label1: TLabel;
    TextToFind: TComboBox;
    Docase: TCheckBox;
    Whole: TCheckBox;
    MatchFirst: TCheckBox;
    IgnoreHTML: TCheckBox;
    Scope: TRadioGroup;
    OkBtn: TButton;
    CancelBtn: TButton;
    Fixed: TCheckBox;
    gbDirection: TGroupBox;
    cbForwardTB: TCheckBox;
    cbForwardLR: TCheckBox;
    cbBackwardBT: TCheckBox;
    cbBackwardRL: TCheckBox;
    Wildcards: TCheckBox;
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure TextToFindChange(Sender: TObject);
    procedure ScopeClick(Sender: TObject);
    procedure cbForwardTBClick(Sender: TObject);
    procedure cbForwardLRClick(Sender: TObject);
    procedure cbBackwardBTClick(Sender: TObject);
    procedure cbBackwardRLClick(Sender: TObject);
    procedure cbForwardTBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbForwardLRMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbBackwardBTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbBackwardRLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    bInhibitcbForwardTB: boolean;
    bInhibitcbForwardLR: boolean;
    bInhibitcbBackwardBT: boolean;
    bInhibitcbBackwardRL: boolean;

  public
    { Public declarations }
    bInhibitToggle: boolean;
    FGrid: TAdvStringGrid;
    FFindDialog: TAdvGridFindDialog;
    FGridCell: TPoint;
    FMsgNoMoreFound: string;
    FMsgNotFound: string;
    FAutoPosition: Boolean;
    FOnFindDone: TNotifyEvent;
    FSelection: TGridRect;

    property OnFindDone: TNotifyEvent read FOnFindDone write FOnFindDone;
  end;

  TCellFoundEvent = procedure(Sender: TObject; Grid: TAdvStringGrid; ACol,ARow: integer; Value: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridFindDialog = class(TComponent)
  private
    FGrid: TAdvStringGrid;
    FAsgFind: TAsgFindDlg;
    FTxtCaption: string;
    FTxtOptionsWholeWords: string;
    FTxtScope: string;
    FTxtDirForward1: string;
    FTxtDirForward2: string;
    FTxtBtnOk: string;
    FTxtScopeCurrCol: string;
    FTxtOptionsCase: string;
    FTxtOptionsFixedCells: string;
    FTxtOptionsMatchFirst: string;
    FTxtScopeAllCells: string;
    FTxtScopeSelectedCells: string;
    FTxtTextToFind: string;
    FTxtScopeCurrRow: string;
    FTxtBtnCancel: string;
    FTxtDirBackward1: string;
    FTxtDirBackward2: string;
    FTxtDirection: string;
    FTxtOptionsIgnoreHTML: string;
    FTxtOptions: string;
    FMsgNoMoreFound: string;
    FMsgNotFound: string;
    FSearchText: string;
    FAutoPosition: Boolean;
    FTxtOptionsWildcards: string;
    FOnFindDone: TNotifyEvent;
    FOnFindClose: TNotifyEvent;
    FOnDialogKeyDown: TKeyEvent;
    FOnCellFound: TCellFoundEvent;
    FDialogWidth: integer;
    procedure SetDialogWidth(const Value: integer);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure FindDone(Sender: TObject);
    procedure FindClose(Sender: TObject; var Action: TCloseAction);
    procedure DialogKeyDown(Sender: TObject;  var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property Dialog: TAsgFindDlg read FAsgFind;
  published
    property AutoPosition: Boolean read FAutoPosition write FAutoPosition;
    property DialogWidth: integer read FDialogWidth write SetDialogWidth default 360;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property MsgNotFound: string read FMsgNotFound write FMsgNotFound;
    property MsgNoMoreFound: string read FMsgNoMoreFound write FMsgNoMoreFound;
    property SearchText: string read FSearchText write FSearchText;
    property TxtCaption: string read FTxtCaption write FTxtCaption;
    property TxtTextToFind: string read FTxtTextToFind write FTxtTextToFind;
    property TxtDirection: string read FTxtDirection write FTxtDirection;
    property TTxtDirForward1: string read FTxtDirForward1 write FTxtDirForward1;
    property TTxtDirForward2: string read FTxtDirForward2 write FTxtDirForward2;
    property TTxtDirBackward1: string read FTxtDirBackward1 write FTxtDirBackward1;
    property TTxtDirBackward2: string read FTxtDirBackward2 write FTxtDirBackward2;
    property TxtScope: string read FTxtScope write FTxtScope;
    property TxtScopeAllCells: string read FTxtScopeAllCells write FTxtScopeAllCells;
    property TxtScopeCurrRow: string read FTxtScopeCurrRow write FTxtScopeCurrRow;
    property TxtScopeCurrCol: string read FTxtScopeCurrCol write FTxtScopeCurrCol;
    property TxtScopeSelectedCells: string read FTxtScopeSelectedCells write FTxtScopeSelectedCells;
    property TxtOptions: string read FTxtOptions write FTxtOptions;
    property TxtOptionsCase: string read FTxtOptionsCase write FTxtOptionsCase;
    property TxtOptionsWholeWords: string read FTxtOptionsWholeWords write FTxtOptionsWholeWords;
    property TxtOptionsMatchFirst: string read FTxtOptionsMatchFirst write FTxtOptionsMatchFirst;
    property TxtOptionsIgnoreHTML: string read FTxtOptionsIgnoreHTML write FTxtOptionsIgnoreHTML;
    property TxtOptionsFixedCells: string read FTxtOptionsFixedCells write FTxtOptionsFixedCells;
    property TxtOptionsWildcards: string read FTxtOptionsWildcards write FTxtOptionsWildcards;
    property TxtBtnOk: string read FTxtBtnOk write FTxtBtnOk;
    property TxtBtnCancel: string read FTxtBtnCancel write FTxtBtnCancel;
    property OnCellFound: TCellFoundEvent read FOnCellFound write FOnCellFound;
    property OnFindDone: TNotifyEvent read FOnFindDone write FOnFindDone;
    property OnFindClose: TNotifyEvent read FOnFindClose write FOnFindClose;
    property OnDialogKeyDown: TKeyEvent read FOnDialogKeyDown write FOnDialogKeyDown;
  end;


implementation


{$R *.DFM}

procedure TAsgFindDlg.OkBtnClick(Sender: TObject);
var
  FirstSearch: Boolean;
  FindParams: TFindParams;
  r: TRect;
  pt1,pt2: TPoint;
begin
  if Assigned(FGrid) then
  begin
    if (TextToFind.Text <> '') and
       (TextToFind.Items.IndexOf(TextToFind.Text) = -1) then
      TextToFind.Items.Add(TextToFind.Text);

    FindParams := [fnAutoGoto];

    if DoCase.Checked then
      FindParams := FindParams + [fnMatchCase];
    if Whole.Checked then
      FindParams := FindParams + [fnMatchFull];
    if MatchFirst.Checked then
      FindParams := FindParams + [fnMatchStart];
    if Wildcards.Checked then
      FindParams := FindParams + [fnMatchRegular];

    if IgnoreHTML.Checked then
      FindParams := FindParams + [fnIgnoreHTMLTags];
    if Fixed.Checked then
      FindParams := FindParams + [fnIncludeFixed];
    if cbBackwardBT.Checked or cbBackwardRL.Checked then
      FindParams := FindParams + [fnBackward];
    if cbForwardLR.Checked or cbBackwardRL.Checked then
      FindParams := FindParams + [fnDirectionLeftRight];
    if Scope.ItemIndex = 1 then
      FindParams := FindParams + [fnFindInCurrentRow];
    if Scope.ItemIndex = 2 then
      FindParams := FindParams + [fnFindInCurrentCol];
    if Scope.ItemIndex = 3 then
    begin
      FindParams := FindParams + [fnSelectedCells];
      FGrid.Selection := FSelection;
    end;

    FirstSearch := (FGridCell.x = -1) and (FGridCell.y = -1);
    FGridCell := FGrid.Find(FGridCell,TextToFind.Text,FindParams);

    if (FGridCell.x = -1) and (FGridCell.y = -1) then
    begin
      if Assigned(FOnFindDone) then
        FOnFindDone(Self)
      else
      begin
        {$IFDEF DELPHI9_LVL}
        Self.FormStyle := fsNormal;
        Application.NormalizeTopMosts;
        {$ENDIF}
        if FirstSearch then
          MessageDlg(FMsgNotFound + ' '#13+'"'+TextToFind.Text+'"',mtInformation,[mbOK],0)
        else
          MessageDlg(FMsgNoMoreFound + ' '#13+'"'+TextToFind.Text+'"',mtInformation,[mbOK],0);
        {$IFDEF DELPHI9_LVL}
        Application.RestoreTopMosts;
        Self.FormStyle := fsStayOnTop;
        {$ENDIF}
      end;
    end
    else
    begin
      if FAutoPosition then
      begin
        r := FGrid.CellRect(FGridCell.x, FGridCell.Y);
        pt1 := Point(r.Left,r.Top);
        pt2 := Point(r.Right,r.Bottom);
        pt1 := FGrid.ClientToScreen(pt1);
        pt2 := FGrid.ClientToScreen(pt2);

        if pt1.y + Height > Screen.Height then
          r.Top := pt2.y - Height
        else
          r.Top := pt1.y;

        if pt1.x + Width > Screen.Width then
          r.Left := pt1.x - Width
        else
          r.Left := pt2.x;

        Left := r.Left;
        Top := r.Top;
      end;

      if Assigned(FFindDialog.FOnCellFound) then
      begin
        FFindDialog.FOnCellFound(self,FGrid, FGridCell.x, FGridCell.y, TextToFind.Text);
      end;
    end;
  end;
end;

procedure TAsgFindDlg.FormCreate(Sender: TObject);
begin
  FGridCell := Point(-1,-1);
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE + SWP_NOMOVE + SWP_NOSIZE);
end;

procedure TAsgFindDlg.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAsgFindDlg.TextToFindChange(Sender: TObject);
begin
  FGridCell := Point(-1,-1);
end;

{ TAdvGridFindDialog }

constructor TAdvGridFindDialog.Create(AOwner: TComponent);
begin
  inherited;
  
  FAsgFind := TAsgFindDlg.Create(nil);
  FAsgFind.FFindDialog := self;

  FDialogWidth := 360;

  FMsgNotFound := 'Could not find text';
  FMsgNoMoreFound := 'No more occurrences of text ';

  FTxtCaption := 'Find text';

  FTxtTextToFind := 'Text to find';

  FTxtDirection := 'Direction';
  FTxtDirForward1 := 'Forward (top to bottom)';
  FTxtDirForward2 := 'Forward (left to right)';
  FTxtDirBackward1 := 'Backward (bottom to top)';
  FTxtDirBackward2 := 'Backward (right to left)';

  FTxtScope := 'Scope';
  FTxtScopeCurrCol := 'Current column only';
  FTxtScopeCurrRow := 'Current row only';
  FTxtScopeAllCells := 'All cells';
  FTxtScopeSelectedCells := 'Selected cells';

  FTxtOptions := 'Options';
  FTxtOptionsCase := '&Case sensitive';
  FTxtOptionsMatchFirst := '&Match from first char';
  FTxtOptionsFixedCells := '&Find in fixed cells';
  FTxtOptionsWholeWords := '&Whole words only';
  FTxtOptionsIgnoreHTML := '&Ignore HTML tags';
  FTxtOptionsWildcards := 'Match with &wildcards';

  FTxtBtnCancel := 'Cancel';
  FTxtBtnOk := 'Ok';
end;

destructor TAdvGridFindDialog.Destroy;
begin
  FAsgFind.Free;
  inherited;
end;

procedure TAdvGridFindDialog.DialogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnDialogKeyDown) then
    OnDialogKeyDown(Self, Key, Shift);
end;

procedure TAdvGridFindDialog.Execute;
begin
  FAsgFind.FGrid := Self.FGrid;
  FAsgFind.Caption := FTxtCaption;
  FAsgFind.FSelection := FGrid.Selection;

  FAsgFind.FAutoPosition := FAutoPosition;

  FAsgFind.Options.Caption := FTxtOptions;

  FAsgFind.Scope.Items[0] := FTxtScopeAllCells;
  FAsgFind.Scope.Items[1] := FTxtScopeCurrRow;
  FAsgFind.Scope.Items[2] := FTxtScopeCurrCol;
  FAsgFind.Scope.Items[3] := FTxtScopeSelectedCells;  

  FAsgFind.Scope.Caption := FTxtScope;
  FAsgFind.gbDirection.Caption := FTxtDirection;

  FAsgFind.Label1.Caption := FTxtTextToFind;

  FAsgFind.Docase.Caption := FTxtOptionsCase;
  FAsgFind.Whole.Caption := FTxtOptionsWholeWords;
  FAsgFind.MatchFirst.Caption := FTxtOptionsMatchFirst;
  FAsgFind.IgnoreHTML.Caption := FTxtOptionsIgnoreHTML;
  FAsgFind.Fixed.Caption := FTxtOptionsFixedCells;
  FAsgFind.Wildcards.Caption := FTxtOptionsWildcards;

  FAsgFind.OkBtn.Caption := TxtBtnOK;
  FAsgFind.CancelBtn.Caption := TxtBtnCancel;

  FAsgFind.cbForwardTB.Caption := FTxtDirForward1;
  FAsgFind.cbForwardLR.Caption := FTxtDirForward2;
  FAsgFind.cbBackwardBT.Caption := FTxtDirBackward1;
  FAsgFind.cbBackwardRL.Caption := FTxtDirBackward2;

  FAsgFind.FMsgNoMoreFound := FMsgNoMoreFound;
  FAsgFind.FMsgNotFound := FMsgNotFound;

  FAsgFind.TextToFind.Text := FSearchText;

  FAsgFind.OnClose := FindClose;

  if Assigned(OnFindDone) then
    FAsgFind.OnFindDone := FindDone
  else
    FAsgFind.OnFindDone := nil;

  FAsgFind.OnKeyDown := DialogKeyDown;

  FAsgFind.Width := FDialogWidth;

  FAsgFind.gbDirection.Width := (FAsgFind.width - 32) div 2;
  FAsgFind.Scope.Width := (FAsgFind.width - 32) div 2;
  FAsgFind.Options.Width := (FAsgFind.width - 32) div 2;
  FAsgFind.Options.Left := FAsgFind.Width - FAsgFind.Options.Width - 16;

  FAsgFind.Show;
end;

procedure TAdvGridFindDialog.FindClose(Sender: TObject;var Action: TCloseAction);
begin
  if Assigned(FOnFindClose) then
    FOnFindClose(Self);
end;

procedure TAdvGridFindDialog.FindDone(Sender: TObject);
begin
  if Assigned(FOnFindDone) then
    FOnFindDone(Self);
end;

procedure TAdvGridFindDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FGrid) then
     FGrid := Nil;
  inherited;
end;

procedure TAdvGridFindDialog.SetDialogWidth(const Value: integer);
begin
  if FDialogWidth > 300 then
    FDialogWidth := Value;
end;

procedure TAsgFindDlg.ScopeClick(Sender: TObject);
begin
  // if "Current row only" selected, disable
  // bottom-to-top & top-to-bottom directions
  if Scope.ItemIndex = 1 then
  begin
    cbForwardTB.Enabled := False;
    cbBackwardBT.Enabled := False;
    cbBackwardRL.Enabled := True;
    cbForwardLR.Enabled := True;
    if (cbForwardTB.Checked or cbBackwardBT.checked) then
    begin
      cbForwardTB.Checked := False;
      cbBackwardBT.Checked := False;
      cbBackwardRL.Checked := False;
      cbForwardLR.Checked := True;
    end;
  end
  else
  // if "Current column only" selected, disable
  // right-to-left & left-to-right directions
  if Scope.ItemIndex = 2 then
  begin
    cbForwardLR.Enabled := False;
    cbBackwardRL.Enabled := False;
    cbBackwardBT.Enabled := True;
    cbForwardTB.Enabled := True;
    if (cbForwardLR.Checked or cbBackwardRL.Checked) then
    begin
      cbForwardLR.Checked := False;
      cbBackwardRL.Checked := False;
      cbBackwardBT.Checked := False;
      cbForwardTB.Checked := True;
    end;
  end
  else
  // otherwise, enable all direction options
  begin
    cbForwardLR.Enabled := True;
    cbBackwardRL.Enabled := True;
    cbForwardTB.Enabled := True;
    cbBackwardBT.Enabled := True;
  end;
end;

// *** The following code block uses a group of four checkboxes to simulate
// *** a radio groupbox with four buttons.  This was done to produce the
// *** mutually exclusive behavior of radio's, while allowing individual
// *** "radio's" to be enabled/disabled.

procedure TAsgFindDlg.cbForwardTBClick(Sender: TObject);
begin
  if cbForwardTB.Checked and not bInhibitcbForwardTB then
  begin
    if cbForwardLR.Checked then
      cbForwardLR.Checked := False;
    if cbBackwardBT.Checked then
      cbBackwardBT.Checked := False;
    if cbBackwardRL.Checked then
      cbBackwardRL.Checked := False;
  end
  else
  if bInhibitcbForwardTB then
  begin
    cbForwardTB.Checked := True;
    bInhibitcbForwardTB := False;
  end;
end;

procedure TAsgFindDlg.cbForwardLRClick(Sender: TObject);
begin
  if cbForwardLR.Checked and not bInhibitcbForwardLR then
  begin
    if cbForwardTB.Checked then
      cbForwardTB.Checked := False;
    if cbBackwardBT.Checked then
     cbBackwardBT.Checked := False;
    if cbBackwardRL.Checked then
      cbBackwardRL.Checked := False;
  end
  else
  if bInhibitcbForwardLR then
  begin
    cbForwardLR.Checked := True;
    bInhibitcbForwardLR := False;
  end;
end;

procedure TAsgFindDlg.cbBackwardBTClick(Sender: TObject);
begin
  if cbBackwardBT.Checked and not bInhibitcbBackwardBT then
  begin
    if cbForwardTB.Checked then
      cbForwardTB.Checked := False;
    if cbForwardLR.Checked then
      cbForwardLR.Checked := False;
    if cbBackwardRL.Checked then
      cbBackwardRL.Checked := False;
  end
  else
  if bInhibitcbBackwardBT then
  begin
    cbBackwardBT.Checked := True;
    bInhibitcbBackwardBT := False;
  end;
end;

procedure TAsgFindDlg.cbBackwardRLClick(Sender: TObject);
begin
  if cbBackwardRL.Checked and not bInhibitcbBackwardRL then
  begin
    if cbForwardTB.Checked then
      cbForwardTB.Checked := False;
    if cbForwardLR.Checked then
      cbForwardLR.Checked := False;
    if cbBackwardBT.Checked then
      cbBackwardBT.Checked := False;
  end
  else
  if bInhibitcbBackwardRL then
  begin
    cbBackwardRL.Checked := True;
    bInhibitcbBackwardRL := False;
  end;
end;

procedure TAsgFindDlg.cbForwardTBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cbForwardTB.Checked then
    bInhibitcbForwardTB := True;
end;

procedure TAsgFindDlg.cbForwardLRMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cbForwardLR.Checked then
    bInhibitcbForwardLR := True;
end;

procedure TAsgFindDlg.cbBackwardBTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cbBackwardBT.Checked then
    bInhibitcbBackwardBT := True;
end;

procedure TAsgFindDlg.cbBackwardRLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cbBackwardRL.Checked then
    bInhibitcbBackwardRL := True;
end;

// ********* end checkbox group with radiobox behaviour *********

end.
