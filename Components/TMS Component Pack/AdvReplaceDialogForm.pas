{*************************************************************************}
{ TMS Replace Dialog component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2012 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvReplaceDialogForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, Buttons, StdCtrls, ExtCtrls;

type
  TAdvReplaceDialogOption = (fdoCurrentFile, fdoSelection, fdoAllOpenFiles, fdoCaseSensitive, fdoExpression,
    fdoWholeWordOnly, fdoWrapAtEndOfFile, fdoMoreEnabled,
    fdoFindEnabled, fdoReplaceAllEnabled, fdoReplaceEnabled,
    fdoCloseEnabled, fdoFindList, fdoMoreExpanded, fdoReplaceList, fdoDown);

  TAdvFindDialogVisibleOption = (fdovCaseSensitive, fdovExpression,
    fdovWholeWordOnly, fdovWrapAtEndOfFile, fdovMore, fdovReplace, fdovReplaceAll, fdovFind,
    fdovClose, fdovFindCombo, fdovFindMemo, fdovReplaceCombo, fdovReplaceMemo, fdovReplaceAllRange, fdovCurrentFile, fdovSelection,
    fdovAllOpenFiles, fdovDirection);


  TAdvReplaceDialogOptions = set of TAdvReplaceDialogOption;
  TAdvReplaceDialogVisibleOptions = set of TAdvFindDialogVisibleOption;

const
  DefaultOptions = [fdoMoreEnabled, fdoReplaceAllEnabled, fdoFindEnabled, fdoReplaceEnabled, fdoCloseEnabled, fdoDown];
  DefaultVisibleOptions = [fdovCaseSensitive, fdovExpression,
    fdovWholeWordOnly, fdovWrapAtEndOfFile, fdovMore, fdovReplace, fdovReplaceAll, fdovFind,
    fdovClose, fdovFindCombo, fdovFindMemo, fdovReplaceCombo, fdovReplaceMemo, fdovReplaceAllRange, fdovCurrentFile, fdovSelection,
    fdovDirection];

type

  TAdvReplaceDialog = class;

  TEditChangeEvent = procedure(Sender: TObject; var AText: string) of object;


  TReplaceDialogForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    SpeedButton1: TSpeedButton;
    PopupMenu1: TPopupMenu;
    abCharacter1: TMenuItem;
    NewLine1: TMenuItem;
    AnyCharacter1: TMenuItem;
    CharacterinRange1: TMenuItem;
    CharacternotinRange1: TMenuItem;
    BeginningofLine1: TMenuItem;
    EndofLine1: TMenuItem;
    aggedExpression1: TMenuItem;
    Or1: TMenuItem;
    N0or1matches1: TMenuItem;
    N1orMoreMatches1: TMenuItem;
    N0or1Matches2: TMenuItem;
    N1: TMenuItem;
    FindBtn: TButton;
    ReplaceBtn: TButton;
    ReplaceAllBtn: TButton;
    CloseBtn: TButton;
    MoreBtn: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    ComboBox2: TComboBox;
    RangeGroup: TRadioGroup;
    Label4: TLabel;
    DirGroup: TRadioGroup;
    procedure SpeedButton1Click(Sender: TObject);
    procedure abCharacter1Click(Sender: TObject);
    procedure NewLine1Click(Sender: TObject);
    procedure AnyCharacter1Click(Sender: TObject);
    procedure CharacterinRange1Click(Sender: TObject);
    procedure CharacternotinRange1Click(Sender: TObject);
    procedure BeginningofLine1Click(Sender: TObject);
    procedure EndofLine1Click(Sender: TObject);
    procedure aggedExpression1Click(Sender: TObject);
    procedure Or1Click(Sender: TObject);
    procedure N0or1matches1Click(Sender: TObject);
    procedure N1orMoreMatches1Click(Sender: TObject);
    procedure N0or1Matches2Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FindBtnClick(Sender: TObject);
    procedure ReplaceBtnClick(Sender: TObject);
    procedure ReplaceAllBtnClick(Sender: TObject);
    procedure MoreBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RangeGroupClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { Private declarations }
    FOnReplace: TNotifyEvent;
    FOnFind: TNotifyEvent;
    FOnReplaceAll: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FDialog: TAdvReplaceDialog;
    FOnFindEditChange: TEditChangeEvent;
    FOnReplaceEditChange: TEditChangeEvent;
    function GetReplaceText: String;
    procedure SetReplaceText(const Value: String);
    function GetFindText: String;
    procedure SetFindText(const Value: String);
  protected
    { Protected declarations }
    procedure DoFindEditChange(var AText: string); virtual;
    procedure DoReplaceEditChange(var AText: string); virtual;
    procedure EnableButtons; virtual;
  public
    { Public declarations }
    property Dialog: TAdvReplaceDialog read FDialog write FDialog;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnReplaceAll: TNotifyEvent read FOnReplaceAll write FOnReplaceAll;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnFindEditChange: TEditChangeEvent read FOnFindEditChange write FOnFindEditChange;
    property OnReplaceEditChange: TEditChangeEvent read FOnReplaceEditChange write FOnReplaceEditChange;
    property ReplaceText: String read GetReplaceText write SetReplaceText;
    property FindText: String read GetFindText write SetFindText;
  end;

  TReplaceUILanguage = class(TPersistent)
  private
    FFindWhat: String;
    FCaseSensitive: String;
    FWrapAtEndOfFile: String;
    FExpression: String;
    FMore: String;
    FWholeWordOnly: String;
    FClose: String;
    FFind: String;
    FCurrentFile: String;
    FReplace: String;
    FSelection: String;
    FReplaceAll: String;
    FAllOpenFiles: String;
    FReplaceWith: String;
    FLess: String;
    FReplaceAllRange: String;
    FDirection: String;
    FDown: String;
    FUp: String;
    FReplaceCaption: string;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property ReplaceCaption: string read FReplaceCaption write FReplaceCaption;
    property FindWhat: String read FFindWhat write FFindWhat;
    property ReplaceWith: String read FReplaceWith write FReplaceWith;
    property CaseSensitive: String read FCaseSensitive write FCaseSensitive;
    property Expression: String read FExpression write FExpression;
    property WholeWordOnly: String read FWholeWordOnly write FWholeWordOnly;
    property WrapAtEndOfFile: String read FWrapAtEndOfFile write FWrapAtEndOfFile;
    property Find: String read FFind write FFind;
    property Replace: String read FReplace write FReplace;
    property ReplaceAll: String read FReplaceAll write FReplaceAll;
    property ReplaceAllRange: String read FReplaceAllRange write FReplaceAllRange;
    property Close: String read FClose write FClose;
    property More: String read FMore write FMore;
    property Less: String read FLess write FLess;
    property CurrentFile: String read FCurrentFile write FCurrentFile;
    property Selection: String read FSelection write FSelection;
    property AllOpenFiles: String read FAllOpenFiles write FAllOpenFiles;
    property Direction: String read FDirection write FDirection;
    property Up: String read FUp write FUp;
    property Down: String read FDown write FDown;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvReplaceDialog = class(TComponent)
  private
    FReplaceText: String;
    FOnReplace: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnReplacePrevious: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOptions: TAdvReplaceDialogOptions;
    FReplaceList: TStringList;
    FOnFind: TNotifyEvent;
    FFindText: String;
    FFindList: TStringList;
    FUILanguage: TReplaceUILanguage;
    FVisibleOptions: TAdvReplaceDialogVisibleOptions;
    FOnFindEditChange: TEditChangeEvent;
    FOnReplaceEditChange: TEditChangeEvent;
    FAutoHistory: boolean;
    FBorderStyle: TFormBorderStyle;
    Frm: TReplaceDialogForm;
    procedure SetUILanguage(const Value: TReplaceUILanguage);
    procedure SetOptions(const Value: TAdvReplaceDialogOptions);
  protected
    procedure DoShow(Sender: TObject);
    procedure DoClose(Sender: TObject);
    procedure DoReplace(Sender: TObject);
    procedure DoReplaceAll(Sender: TObject);
    procedure DoFind(Sender: TObject);
    procedure DoFindEditChange(Sender: TObject; var AText: string);
    procedure DoReplaceEditChange(Sender: TObject; var AText: string);
    function CreateDialogForm(AOwner: TComponent): TReplaceDialogForm; virtual;
    function InitDialog: TCustomForm; virtual;
  public
    procedure Execute; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Form: TReplaceDialogForm read Frm;
  published
    property AutoHistory: boolean read FAutoHistory write FAutoHistory default true;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog;
    property ReplaceText: String read FReplaceText write FReplaceText;
    property FindText: String read FFindText write FFindText;
    property ReplaceList: TStringList read FReplaceList write FReplaceList;
    property FindList: TStringList read FFindList write FFindList;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnFindEditChange: TEditChangeEvent read FOnFindEditChange write FOnFindEditChange;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
    property OnReplaceEditChange: TEditChangeEvent read FOnReplaceEditChange write FOnReplaceEditChange;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnReplaceAll: TNotifyEvent read FOnReplacePrevious write FOnReplacePrevious;
    property Options: TAdvReplaceDialogOptions read FOptions write SetOptions default DefaultOptions;
    property VisibleOptions: TAdvReplaceDialogVisibleOptions read FVisibleOptions write FVisibleOptions default DefaultVisibleOptions;
    property UILanguage: TReplaceUILanguage read FUILanguage write SetUILanguage;
  end;

var
  ReplaceDialogForm: TReplaceDialogForm;
  More: Boolean;
  FPrevCap: String;

{$R *.dfm}

implementation

{ TAdvReplaceDialog }

constructor TAdvReplaceDialog.Create(AOwner: TComponent);
begin
  inherited;
  FUILanguage := TReplaceUILanguage.Create(Self);
  FOptions := DefaultOptions;
  FVisibleOptions := DefaultVisibleOptions;
  FReplaceList := TStringList.Create;
  FFindList := TStringList.Create;
  FAutoHistory := true;
  FBorderStyle := bsDialog;
end;

destructor TAdvReplaceDialog.Destroy;
begin
  FUILanguage.Free;
  FReplaceList.Free;
  FFindList.Free;
  inherited;
end;

procedure TAdvReplaceDialog.DoClose(Sender: TObject);
begin
  if FAutoHistory then
  begin
    if FFindList.IndexOf(FindText) = -1 then
      FFindList.Add(FindText);

    if FReplaceList.IndexOf(ReplaceText) = -1 then
      FReplaceList.Add(ReplaceText);
  end;

  if Assigned(OnClose) then
    OnClose(Sender);
end;

procedure TAdvReplaceDialog.DoReplace(Sender: TObject);
begin
  if Assigned(OnReplace) then
    OnReplace(Sender);
end;

procedure TAdvReplaceDialog.DoReplaceAll(Sender: TObject);
begin
  if Assigned(OnReplaceAll) then
    OnReplaceAll(Sender);
end;

procedure TAdvReplaceDialog.DoReplaceEditChange(Sender: TObject;
  var AText: string);
begin
  if Assigned(FOnReplaceEditChange) then
    FOnReplaceEditChange(Self, AText);
end;

procedure TAdvReplaceDialog.DoFind(Sender: TObject);
begin
  if Assigned(OnFind) then
    OnFind(Sender);
end;

procedure TAdvReplaceDialog.DoFindEditChange(Sender: TObject;
  var AText: string);
begin
  if Assigned(FOnFindEditChange) then
    FOnFindEditChange(Self, AText);
end;

function TAdvReplaceDialog.CreateDialogForm(
  AOwner: TComponent): TReplaceDialogForm;
begin
  Result := TReplaceDialogForm.Create(AOwner);
end;

procedure TAdvReplaceDialog.DoShow(Sender: TObject);
begin
  if Assigned(OnShow) then
    OnShow(Self);
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));

  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function TAdvReplaceDialog.InitDialog: TCustomForm;
const
  DirHeight = 50;
var
  DialogUnits: TPoint;
begin
  frm := CreateDialogForm(Self);
  frm.Dialog := Self;
  frm.ReplaceText := ReplaceText;
  frm.FindText := FindText;
  frm.OnClose := DoClose;
  frm.OnShow := DoShow;
  frm.OnReplace := DoReplace;
  frm.OnReplaceAll := DoReplaceAll;
  frm.OnFind := DoFind;
  frm.OnFindEditChange := DoFindEditChange;
  frm.OnReplaceEditChange := DoReplaceEditChange;

  frm.CheckBox1.Checked := fdoCaseSensitive in Options;
  frm.CheckBox2.Checked := fdoExpression in Options;
  frm.CheckBox4.Checked := fdoWholeWordOnly in Options;
  frm.CheckBox5.Checked := fdoWrapAtEndOfFile in Options;
  frm.FindBtn.Enabled := fdoFindEnabled in Options;
  frm.ReplaceBtn.Enabled := fdoReplaceEnabled in Options;
  frm.ReplaceAllBtn.Enabled := fdoReplaceAllEnabled in Options;
  frm.CloseBtn.Enabled := fdoCloseEnabled in Options;
  frm.MoreBtn.Enabled := fdoMoreEnabled in Options;

  frm.CheckBox1.Visible := fdovCaseSensitive in VisibleOptions;
  frm.CheckBox2.Visible := fdovExpression in VisibleOptions;
  frm.CheckBox4.Visible := fdovWholeWordOnly in VisibleOptions;
  frm.CheckBox5.Visible := fdovWrapAtEndOfFile in VisibleOptions;
  frm.FindBtn.Visible := fdovFind in VisibleOptions;
  frm.ReplaceBtn.Visible := fdovReplace in VisibleOptions;
  frm.ReplaceAllBtn.Visible := fdovReplaceAll in VisibleOptions;
  frm.CloseBtn.Visible := fdovClose in VisibleOptions;
  frm.MoreBtn.Visible := fdovMore in VisibleOptions;

  frm.CheckBox1.Caption := UILanguage.CaseSensitive;
  frm.CheckBox2.Caption := UILanguage.Expression;
  frm.CheckBox4.Caption := UILanguage.WholeWordOnly;
  frm.CheckBox5.Caption := UILanguage.WrapAtEndOfFile;
  frm.FindBtn.Caption := UILanguage.Find;
  frm.ReplaceBtn.Caption := UILanguage.Replace;
  frm.ReplaceAllBtn.Caption := UILanguage.ReplaceAll;
  frm.CloseBtn.Caption := UILanguage.Close;
  frm.MoreBtn.Caption := UILanguage.More;

  if not (fdovAllOpenFiles in VisibleOptions) then
    frm.RangeGroup.Items.Delete(2);

  if not (fdovSelection in VisibleOptions) then
    frm.RangeGroup.Items.Delete(1);

  if not (fdovCurrentFile in VisibleOptions) then
    frm.RangeGroup.Items.Delete(0);

  frm.RangeGroup.Visible := (fdovReplaceAllRange in VisibleOptions) and (frm.RangeGroup.Items.Count > 0);

  frm.RangeGroup.Caption := UILanguage.ReplaceAllRange;

  if frm.RangeGroup.Items.Count > 0 then
    frm.RangeGroup.Items[0] := UILanguage.CurrentFile;
  if frm.RangeGroup.Items.Count > 1 then
    frm.RangeGroup.Items[1] := UILanguage.Selection;
  if frm.RangeGroup.Items.Count > 2 then
    frm.RangeGroup.Items[2] := UILanguage.AllOpenFiles;

  frm.RangeGroup.Caption := UILanguage.ReplaceAllRange;
  if fdoCurrentFile in Options then
    frm.RangeGroup.ItemIndex := 0;

  if fdoSelection in Options then
    frm.RangeGroup.ItemIndex := 1;

  if fdoAllOpenFiles in Options then
    frm.RangeGroup.ItemIndex := 2;

  frm.Label1.Caption := UILanguage.FindWhat;
  frm.Label2.Caption := UILanguage.FindWhat;

  frm.Label3.Caption := UILanguage.ReplaceWith;
  frm.Label4.Caption := UILanguage.ReplaceWith;

  if fdoFindList in Options then
    frm.ComboBox1.Style := csDropDownList;

  if fdoReplaceList in Options then
    frm.ComboBox2.Style := csDropDownList;

  frm.ComboBox1.Items.Assign(FindList);
  frm.ComboBox2.Items.Assign(ReplaceList);

  frm.ComboBox1.Visible := fdovFindCombo in VisibleOptions;
  frm.ComboBox2.Visible := fdovReplaceCombo in VisibleOptions;
  frm.Memo1.Visible := fdovFindMemo in VisibleOptions;
  frm.Memo2.Visible := fdovReplaceMemo in VisibleOptions;

  frm.DirGroup.Visible := fdovDirection in VisibleOptions;

  frm.DirGroup.Items[0] := UILanguage.Down;
  frm.DirGroup.Items[1] := UILanguage.Up;
  frm.DirGroup.Caption := UILanguage.Direction;

  DialogUnits := GetAveCharSize(frm.Canvas);

  if frm.DirGroup.Visible then
    frm.Height := MulDiv(142, DialogUnits.Y, 8)
  else
    frm.Height := MulDiv(111, DialogUnits.Y, 8);

  if not frm.DirGroup.Visible then
  begin
    frm.MoreBtn.Top := frm.MoreBtn.top - DirHeight;
    frm.CloseBtn.Top := frm.CloseBtn.Top - DirHeight;
    frm.RangeGroup.Top := frm.RangeGroup.Top - DirHeight;
    frm.Memo1.Top := frm.Memo1.Top - DirHeight;
    frm.Memo2.Top := frm.Memo2.Top - DirHeight;
    frm.label2.Top := frm.Label2.Top - DirHeight;
    frm.label4.Top := frm.Label4.Top - DirHeight;
  end;

  frm.Caption := UILanguage.ReplaceCaption;

  Result := frm;
end;

procedure TAdvReplaceDialog.Execute;
var
  frm: TReplaceDialogForm;
begin
  frm := InitDialog as TReplaceDialogForm;
  frm.BorderStyle := BorderStyle;
  frm.EnableButtons;
  frm.FormStyle := fsStayOnTop;
  frm.Show;
end;

procedure TAdvReplaceDialog.SetOptions(const Value: TAdvReplaceDialogOptions);
var
  iCount: Integer;
  FOldValues: TAdvReplaceDialogOptions;
begin
  FOldValues := FOptions;
  FOptions := Value;
  iCount := 0;
  if (fdoCurrentFile in Options) then
    Inc(iCount);

  if (fdoSelection in Options) then
    Inc(iCount);

  if (fdoAllOpenFiles in Options) then
    Inc(iCount);

  if iCount > 1 then
    FOptions := FOldValues;
end;

procedure TAdvReplaceDialog.SetUILanguage(const Value: TReplaceUILanguage);
begin
  if FUILanguage <> Value then
    FUILanguage.Assign(Value);
end;

{ TReplaceUILanguage }

procedure TReplaceUILanguage.Assign(Source: TPersistent);
begin
  if (Source is TReplaceUILanguage) then
  begin
    FFindWhat :=  (Source as TReplaceUILanguage).FindWhat;
    FReplace  := (Source as TReplaceUILanguage).Replace;
    FReplaceAll := (Source as TReplaceUILanguage).ReplaceAll;
    FReplaceWith  := (Source as TReplaceUILanguage).ReplaceWith;
    FReplaceAllRange := (Source as TReplaceUILanguage).ReplaceAllRange;
    FCaseSensitive := (Source as TReplaceUILanguage).CaseSensitive;
    FExpression := (Source as TReplaceUILanguage).Expression;
    FWholeWordOnly := (Source as TReplaceUILanguage).WholeWordOnly;
    FWrapAtEndOfFile := (Source as TReplaceUILanguage).WrapAtEndOfFile;
    FFind := (Source as TReplaceUILanguage).Find;
    FClose := (Source as TReplaceUILanguage).Close;
    FMore := (Source as TReplaceUILanguage).More;
    FLess := (Source as TReplaceUILanguage).Less;
    FSelection := (Source as TReplaceUILanguage).Selection;
    FCurrentFile := (Source as TReplaceUILanguage).CurrentFile;
    FAllOpenFiles := (Source as TReplaceUILanguage).AllOpenFiles;
    FDirection := (Source as TReplaceUILanguage).Direction;
    FUp := (Source as TReplaceUILanguage).Up;
    FDown := (Source as TReplaceUILanguage).Down;
  end;
end;

constructor TReplaceUILanguage.Create(AOwner: TComponent);
begin
  inherited Create;
  if(csDesigning in AOwner.ComponentState) and not
    ((csReading in AOwner.ComponentState) or (csLoading in AOwner.ComponentState)) then
  begin
    FReplaceCaption := 'Replace';
    FCaseSensitive := '&Case sensitive';
    FExpression := 'E&xpression';
    FWholeWordOnly := '&Whole word Only';
    FWrapAtEndOfFile := 'Wrap at the end of file';
    FFind := '&Find';
    FFindWhat := 'Find what:';
    FReplaceWith := 'Replace with:';
    FReplaceAll := 'Replace &All';
    FReplace := '&Replace';
    FClose := '&Close';
    FMore := 'Mor&e';
    FLess := 'L&ess';
    FSelection := '&Selection';
    FCurrentFile := 'Curre&nt File';
    FAllOpenFiles := 'All &open files';
    FReplaceAllRange := 'Replace all range';
    FDirection := '&Direction';
    FUp := 'Up';
    FDown := 'Down';
  end;
end;

procedure TReplaceDialogForm.abCharacter1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '\t';
end;

procedure TReplaceDialogForm.aggedExpression1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '()';
end;

procedure TReplaceDialogForm.AnyCharacter1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '.';
end;

procedure TReplaceDialogForm.BeginningofLine1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '^';
end;

procedure TReplaceDialogForm.FindBtnClick(Sender: TObject);
begin
  if Assigned(OnFind) then
    OnFind(Self);
end;

procedure TReplaceDialogForm.ReplaceBtnClick(Sender: TObject);
begin
  if Assigned(OnReplace) then
    OnReplace(Self);
end;

procedure TReplaceDialogForm.ReplaceAllBtnClick(Sender: TObject);
begin
  if Assigned(OnReplaceAll) then
    OnReplaceAll(Self);
end;

procedure TReplaceDialogForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TReplaceDialogForm.MoreBtnClick(Sender: TObject);
var
  heightopen, heightclosed: integer;
  DialogUnits: TPoint;
begin
  DialogUnits := GetAveCharSize(Canvas);

  if DirGroup.Visible then
  begin
    heightopen := MulDiv(274, DialogUnits.Y, 8);
    heightclosed := MulDiv(142, DialogUnits.Y, 8);
  end
  else
  begin
    heightopen := MulDiv(243, DialogUnits.Y, 8);
    heightclosed := MulDiv(111, DialogUnits.Y, 8);
  end;

  if not More then
  begin
    Height := heightopen;
    MoreBtn.Caption := Dialog.UILanguage.Less;
    Dialog.Options := Dialog.Options + [fdoMoreExpanded];
  end
  else
  begin
    MoreBtn.Caption := Dialog.UILanguage.More;
    Height := heightclosed;
    Dialog.Options := Dialog.Options - [fdoMoreExpanded];
  end;
  Label1.Enabled := More;
  ComboBox1.Enabled := More;
  Label3.Enabled := More;
  ComboBox2.Enabled := More;
  More := not More;
end;

procedure TReplaceDialogForm.CharacterinRange1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '[]';
end;

procedure TReplaceDialogForm.CharacternotinRange1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '[^]';
end;

procedure TReplaceDialogForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Dialog.Options := Dialog.Options + [fdoCaseSensitive]
  else
    Dialog.Options := Dialog.Options - [fdoCaseSensitive];
end;

procedure TReplaceDialogForm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    Dialog.Options := Dialog.Options + [fdoExpression]
  else
    Dialog.Options := Dialog.Options - [fdoExpression];
end;

procedure TReplaceDialogForm.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    Dialog.Options := Dialog.Options + [fdoWholeWordOnly]
  else
    Dialog.Options := Dialog.Options - [fdoWholeWordOnly];
end;

procedure TReplaceDialogForm.CheckBox5Click(Sender: TObject);
begin
  if CheckBox5.Checked then
    Dialog.Options := Dialog.Options + [fdoWrapAtEndOfFile]
  else
    Dialog.Options := Dialog.Options - [fdoWrapAtEndOfFile];
end;

procedure TReplaceDialogForm.ComboBox1Change(Sender: TObject);
var
  s: string;
begin
  s := ComboBox1.Text;
  DoFindEditChange(s);
  Dialog.FindText := s;
  EnableButtons;
end;

procedure TReplaceDialogForm.ComboBox2Change(Sender: TObject);
var
  s: string;
begin
  s := ComboBox2.Text;
  DoReplaceEditChange(s);
  Dialog.ReplaceText := s;
end;

procedure TReplaceDialogForm.DoFindEditChange(var AText: string);
begin
  if Assigned(FOnFindEditChange) then
    FOnFindEditChange(Self, AText);
end;

procedure TReplaceDialogForm.DoReplaceEditChange(var AText: string);
begin
  if Assigned(FOnReplaceEditChange) then
    FOnReplaceEditChange(Self, AText);
end;

procedure TReplaceDialogForm.EnableButtons;
begin
  FindBtn.Enabled := (Dialog.FindText <> '') and (fdoFindEnabled in Dialog.Options);
  ReplaceBtn.Enabled := (Dialog.FindText <> '') and (fdoReplaceEnabled in Dialog.Options);
  ReplaceAllBtn.Enabled := (Dialog.FindText <> '') and (fdoReplaceAllEnabled in Dialog.Options);
end;

procedure TReplaceDialogForm.EndofLine1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '$';
end;

procedure TReplaceDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(OnClose) then
    OnClose(Self);
end;

procedure TReplaceDialogForm.FormCreate(Sender: TObject);
var
  DialogUnits: TPoint;
begin
  More := False;
  DialogUnits := GetAveCharSize(Canvas);
  Height := MulDiv(105, DialogUnits.Y, 8);
end;

procedure TReplaceDialogForm.FormShow(Sender: TObject);
begin
  if (fdoMoreExpanded in Dialog.Options) then
  begin
    if not More then
      MoreBtnClick(Self);
  end
  else
  begin
    if More then
      MoreBtnClick(Self);
  end;
end;

function TReplaceDialogForm.GetFindText: String;
begin
  if not More then
    Result := ComboBox1.Text
  else
    Result := Memo1.Text;
end;

function TReplaceDialogForm.GetReplaceText: String;
begin
  if not More then
    Result := ComboBox2.Text
  else
    Result := Memo2.Text;
end;

procedure TReplaceDialogForm.Memo1Change(Sender: TObject);
var
  s: string;
begin
  s := Memo1.Lines.Text;
  DoFindEditChange(s);
  Dialog.FindText := s;
  EnableButtons;
end;

procedure TReplaceDialogForm.N0or1matches1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '*';
end;

procedure TReplaceDialogForm.N0or1Matches2Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '?';
end;

procedure TReplaceDialogForm.N1orMoreMatches1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '+';
end;

procedure TReplaceDialogForm.NewLine1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '\n';
end;

procedure TReplaceDialogForm.Or1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '|';
end;

procedure TReplaceDialogForm.RangeGroupClick(Sender: TObject);
begin
  Dialog.Options := Dialog.Options - [fdoCurrentFile, fdoSelection, fdoAllOpenFiles];
  case RangeGroup.ItemIndex of
  0:Dialog.Options := Dialog.Options + [fdoCurrentFile];
  1:Dialog.Options := Dialog.Options + [fdoSelection];
  2:Dialog.Options := Dialog.Options + [fdoAllOpenFiles];
  end;
end;

procedure TReplaceDialogForm.SetFindText(const Value: String);
begin
  if not More then
    ComboBox1.Text := Value
  else
    Memo1.Text := Value;
end;

procedure TReplaceDialogForm.SetReplaceText(const Value: String);
begin
  if not More then
    ComboBox2.Text := Value
  else
    Memo2.Text := Value;
end;

procedure TReplaceDialogForm.SpeedButton1Click(Sender: TObject);
var
   p : TPoint;
begin
 with Sender as TSpeedButton do
 begin
    GetCursorPos(p);
    p.x := Left + 1;
    p.y := Top + Height + 1;
    p := Self.ClientToScreen(p);
    PopupMenu1.Popup(p.x, p.y);
  end;
end;

end.
