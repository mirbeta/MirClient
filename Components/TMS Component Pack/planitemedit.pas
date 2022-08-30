{***********************************************************************}
{ TPlanner component                                                    }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2013                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

{$I TMSDEFS.INC}

unit PlanItemEdit;

interface
{
    Alarm
      - .Active
      - .Address
      - .Message
      - .TimeBefore

    .Flashing
    .Color / .SelectColor / .SelectFontColor / .TrackColor
    .Font
    .TrackVisible
    .URL
    .Caption
    .Text

    .ImageID (wat voor selectie component)

    .Alignment left/right/centered
}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Planner, StdCtrls, ExtCtrls, ComCtrls, Buttons, CommCtrl,
  PlanRecurrEdit, PlanUtil
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TEditState = (esEdit, esNew);

type
  TBasePlannerItemEditForm = class(TForm)
  private
    FEditState: TEditState;
  protected
    procedure AssignFromPlannerItem(PlannerItem: TPlannerItem); virtual;
    procedure AssignToPlannerItem(PlannerItem: TPlannerItem); virtual;
    procedure CheckInputValues(PlannerItem: TPlannerItem); virtual;
    procedure EditStateChanged; virtual;
    procedure FixEnabledComponents; virtual;
    procedure InternalEditModal(PlannerItem: TPlannerItem); virtual;
    procedure ProcessWarnings; virtual;
    procedure SetEditState(const Value: TEditState); virtual;
  public
    procedure EditModal(PlannerItem: TPlannerItem); virtual;
    property EditState: TEditState read FEditState write SetEditState;
  end;

  TDefaultPlannerItemEditForm = class(TBasePlannerItemEditForm)
    lbl_subject: TLabel;
    lbl_from: TLabel;
    lbl_date: TLabel;
    lbl_notes: TLabel;
    lbl_to: TLabel;
    Label6: TLabel;
    StartTimeDateTimePicker: TDateTimePicker;
    EndTimeDateTimePicker: TDateTimePicker;
    SubjectEdit: TEdit;
    NotesMemo: TMemo;
    PlanDateDateTimePicker: TDateTimePicker;
    ColorDialog: TColorDialog;
    WarningPanel: TPanel;
    ButtonBottomPanel: TPanel;
    ButtonBottomRightPanel: TPanel;
    OKBtn: TButton;
    CancBtn: TButton;
    lbl_url: TLabel;
    UrlEdit: TEdit;
    Bevel1: TBevel;
    chk_reminder: TCheckBox;
    ReminderDateTimePicker: TDateTimePicker;
    lbl_msg: TLabel;
    ReminderMesssageEdit: TEdit;
    Bevel2: TBevel;
    lbl_Colors: TLabel;
    lbl_shape: TLabel;
    ShapeComboBox: TComboBox;
    chk_flashing: TCheckBox;
    FontButton: TButton;
    FontDialog: TFontDialog;
    lbl_address: TLabel;
    ReminderAddressEdit: TEdit;
    FontPanel: TPanel;
    AlarmImage: TImage;
    CautionImage: TImage;
    ImageSpeedButton: TSpeedButton;
    ImageUpDown: TUpDown;
    BackgroundShape: TShape;
    SelectedBackgroundShape: TShape;
    TrackbarShape: TShape;
    SelectedFontShape: TShape;
    lbl_Background: TLabel;
    lbl_Trackbar: TLabel;
    lbl_selectedbackground: TLabel;
    lbl_selectedfont: TLabel;
    btnLeft: TSpeedButton;
    btnCenter: TSpeedButton;
    btnRight: TSpeedButton;
    SpeedButton1: TSpeedButton;
    PlannerRecurrencyEditor1: TPlannerRecurrencyEditor;
    procedure chk_reminderClick(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure PlanDateDateTimePickerChange(Sender: TObject);
    procedure StartTimeDateTimePickerChange(Sender: TObject);
    procedure EndTimeDateTimePickerChange(Sender: TObject);
    procedure ImageUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure AlignLeftBitBtnClick(Sender: TObject);
    procedure AlignCentreLeftBitBtnClick(Sender: TObject);
    procedure AlignRightBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BackgroundShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnLeftClick(Sender: TObject);
    procedure btnCenterClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FImageList: TImageList;
  protected
    function GetEndTime: TTime; virtual;
    function GetPlanDate: TDateTime; virtual;
    function GetStartTime: TTime; virtual;
    function GetReminderTime: TTime; virtual;
    procedure SetEndTime(const Value: TTime); virtual;
    procedure SetImageList(const Value: TImageList); virtual;
    procedure SetPlanDate(const Value: TDateTime); virtual;
    procedure SetStartTime(const Value: TTime); virtual;
    procedure SetReminderTime(const Value: TTime); virtual;
  protected
    property PlanDate: TDateTime read GetPlanDate write SetPlanDate;
    property StartTime: TTime read GetStartTime write SetStartTime;
    property EndTime: TTime read GetEndTime write SetEndTime;
    property ImageList: TImageList read FImageList write SetImageList;
    property ReminderTime: TTime read GetReminderTime write SetReminderTime;
  protected
    procedure AssignFromPlannerItem(PlannerItem: TPlannerItem); override;
    procedure AssignToPlannerItem(PlannerItem: TPlannerItem); override;
    procedure CheckInputValues(PlannerItem: TPlannerItem); override;
    procedure CopyImageToSpeedButton(Index: Integer); virtual;
    procedure EditStateChanged; override;
    procedure FixEnabledComponents; override;
    procedure ImageChanged; virtual;
    procedure ProcessWarnings; override;
  public
    Recurrency: string;
  end;

  TDefaultItemEditor = class(TCustomItemEditor)
  private
    FEditForm: TDefaultPlannerItemEditForm;
    FCenter: Boolean;
    FFormLeft: Integer;
    FFormTop: Integer;
    FLblSubject: string;
    FLblWarning: string;
    FLblTo: string;
    FLblDate: string;
    FLblNotes: string;
    FLblShape: string;
    FLblColor: string;
    FLblFrom: string;
    FLblURL: string;
    FLblReminder: string;
    FLblFlashing: string;
    FLblMessage: string;
    FLblAddress: string;
    FLblSelectedBackground: string;
    FLblSelectedFont: string;
    FLblTrackbar: string;
    FLblBackground: string;
    FRecurrencyDialog: TRecurrencyDialogLanguage;
    procedure SetRecurencyDialog(const Value: TRecurrencyDialogLanguage);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateEditor(AOwner: TComponent); override;
    procedure DestroyEditor; override;
    destructor Destroy; override;
    function Execute: Integer; override;
    procedure PlannerItemToEdit(APlannerItem: TPlannerItem); override;
    procedure EditToPlannerItem(APlannerItem: TPlannerItem); override;
  published
    property CenterOnScreen: Boolean read FCenter write FCenter default True;
    property FormLeft: Integer read FFormLeft write FFormLeft;
    property FormTop: Integer read FFormTop write FFormTop;
    property RecurrencyDialog: TRecurrencyDialogLanguage read FRecurrencyDialog write SetRecurencyDialog;
    property LblWarning: string read FLblWarning write FLblWarning;
    property LblSubject: string read FLblSubject write FLblSubject;
    property LblDate: string read FLblDate write FLblDate;
    property LblFrom: string read FLblFrom write FLblFrom;
    property LblTo: string read FLblTo write FLblTo;
    property LblColor: string read FLblColor write FLblColor;
    property LblShape: string read FLblShape write FLblShape;
    property LblNotes: string read FLblNotes write FLblNotes;
    property LblFlashing: string read FLblFlashing write FLblFlashing;
    property LblReminder: string read FLblReminder write FLblReminder;
    property LblMessage: string read FLblMessage write FLblMessage;
    property LblURL: string read FLblURL write FLblURL;
    property LblAddress: string read FLblAddress write FLblAddress;
    property LblBackground: string read FLblBackground write FLblBackground;
    property LblTrackbar: string read FLblTrackbar write FLblTrackbar;
    property LblSelectedBackground: string read FLblSelectedBackground write FLblSelectedBackground;
    property LblSelectedFont: string read FLblSelectedFont write FLblSelectedFont;
  end;


const
  OneDay = 1;
  OneHour = OneDay / 24;
  OneMinute = OneHour / 60;
  OneSecond = OneMinute / 60;
  MinDuration = 15 * OneMinute;

implementation

{$R *.dfm}

{ TBasePlannerItemEditForm }

procedure TBasePlannerItemEditForm.AssignFromPlannerItem(
  PlannerItem: TPlannerItem);
begin
  //placeholder for descendants
end;

procedure TBasePlannerItemEditForm.AssignToPlannerItem(
  PlannerItem: TPlannerItem);
begin
  //placeholder for descendants
end;

procedure TBasePlannerItemEditForm.CheckInputValues(PlannerItem: TPlannerItem);
begin
  //placeholder for descendants
end;

procedure TBasePlannerItemEditForm.EditModal(PlannerItem: TPlannerItem);
begin
  Assert(Assigned(PlannerItem), 'Must pass a non-nil planner item for editing');
  EditState := esEdit;
  InternalEditModal(PlannerItem);
end;

procedure TBasePlannerItemEditForm.EditStateChanged;
begin
  //placeholder for descendants
end;

procedure TBasePlannerItemEditForm.FixEnabledComponents;
begin
  //placeholder for descendants
end;

procedure TBasePlannerItemEditForm.InternalEditModal(
  PlannerItem: TPlannerItem);
begin
  AssignFromPlannerItem(PlannerItem);
  FixEnabledComponents;
  ProcessWarnings;
  if (ShowModal = mrOK) then
  begin
    CheckInputValues(PlannerItem);
    AssignToPlannerItem(PlannerItem);
    PlannerItem.Update;
  end;
end;

procedure TBasePlannerItemEditForm.ProcessWarnings;
begin
  //placeholder for descendants
end;

procedure TBasePlannerItemEditForm.SetEditState(const Value: TEditState);
begin
  if Value <> EditState then
  begin
    FEditState := Value;
    EditStateChanged;
  end;
end;


{ TDefaultPlannerItemEditForm }

procedure TDefaultPlannerItemEditForm.AlignCentreLeftBitBtnClick(
  Sender: TObject);
begin
  NotesMemo.Alignment := taCenter;
end;

procedure TDefaultPlannerItemEditForm.AlignLeftBitBtnClick(
  Sender: TObject);
begin
  NotesMemo.Alignment := taLeftJustify;
end;

procedure TDefaultPlannerItemEditForm.AlignRightBitBtnClick(
  Sender: TObject);
begin
  NotesMemo.Alignment := taRightJustify;
end;

procedure TDefaultPlannerItemEditForm.AssignFromPlannerItem(
  PlannerItem: TPlannerItem);
begin
  inherited;
  SubjectEdit.Text := PlannerItem.CaptionText;
  if PlannerItem.RealTime then
  begin
    PlanDate := PlannerItem.ItemRealStartTime;
    StartTime := PlannerItem.ItemRealStartTime;
    EndTime := PlannerItem.ItemRealEndTime;
  end
  else
  begin
    PlanDate := PlannerItem.ItemStartTime;
    StartTime := PlannerItem.ItemStartTime;
    EndTime := PlannerItem.ItemEndTime;
  end;

  NotesMemo.Lines := PlannerItem.Text;
  NotesMemo.Alignment := PlannerItem.Alignment;

  btnLeft.Down := PlannerItem.Alignment = taLeftJustify;
  btnCenter.Down := PlannerItem.Alignment = taCenter;
  btnRight.Down := PlannerItem.Alignment = taRightJustify;

  // three possible shapes: 1-psRect, 2-psRounded, 3-psHexagon
  ShapeComboBox.ItemIndex := Ord(PlannerItem.Shape);
  chk_Flashing.Checked := PlannerItem.Flashing;
  chk_Flashing.Enabled := PlannerItem.Planner.EnableFlashing;

  UrlEdit.Text := PlannerItem.URL;

  chk_Reminder.Checked := PlannerItem.Alarm.Active;
  ReminderTime := PlannerItem.Alarm.TimeBefore;
  ReminderMesssageEdit.Text := PlannerItem.Alarm.Message;
  ReminderAddressEdit.Text := PlannerItem.Alarm.Address;

  BackgroundShape.Brush.Color := PlannerItem.Color;
  TrackbarShape.Brush.Color := PlannerItem.TrackColor;
  SelectedBackgroundShape.Brush.Color := PlannerItem.SelectColor;
  SelectedFontShape.Brush.Color := PlannerItem.SelectFontColor;

  Recurrency := PlannerItem.Recurrency;
  FontPanel.Font := PlannerItem.Font;

  // First assign ImageList, because that will set the Min/Max values of the ImageUpDown
  ImageList := PlannerItem.Owner.PlannerImages;
  if Assigned(ImageList) then
  begin
    ImageUpDown.Position := PlannerItem.ImageID;
    ImageChanged;
  end;
end;

procedure TDefaultPlannerItemEditForm.AssignToPlannerItem(
  PlannerItem: TPlannerItem);
begin
  inherited;
  PlannerItem.CaptionText := SubjectEdit.Text;
  PlannerItem.ItemStartTime := PlanDate + StartTime;
  PlannerItem.ItemEndTime := PlanDate + EndTime;
  PlannerItem.ItemRealStartTime := PlanDate + StartTime;
  PlannerItem.ItemRealEndTime := PlanDate + EndTime;
  PlannerItem.RealTime := True;
  PlannerItem.Text := TStringList(NotesMemo.Lines);
  PlannerItem.Alignment := NotesMemo.Alignment;

  PlannerItem.Shape := TPlannerShape(ShapeComboBox.ItemIndex);
  PlannerItem.Flashing := chk_Flashing.Checked;

  PlannerItem.URL := UrlEdit.Text;

  PlannerItem.Alarm.Active := chk_Reminder.Checked;
  PlannerItem.Alarm.TimeBefore := ReminderTime;
  PlannerItem.Alarm.Message := ReminderMesssageEdit.Text;
  PlannerItem.Alarm.Address := ReminderAddressEdit.Text;

  PlannerItem.Color := BackgroundShape.Brush.Color;
  PlannerItem.TrackColor := TrackbarShape.Brush.Color;
  PlannerItem.SelectColor := SelectedBackgroundShape.Brush.Color;
  PlannerItem.SelectFontColor := SelectedFontShape.Brush.Color;

  PlannerItem.Font := FontPanel.Font;

  PlannerItem.Recurrency := Recurrency;

  if Assigned(ImageList) then
    PlannerItem.ImageID := ImageUpDown.Position;

  PlannerItem.Update;
end;

procedure TDefaultPlannerItemEditForm.BackgroundShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TShape then
  begin
    ColorDialog.Color := (Sender as TShape).Brush.Color;
    if ColorDialog.Execute then
      (Sender as TShape).Brush.Color := ColorDialog.Color;
  end;
end;

procedure TDefaultPlannerItemEditForm.CheckInputValues(PlannerItem: TPlannerItem);
begin
  inherited;
  // three possible shapes: 1-psRect, 2-psRounded, 3-psHexagon
  Assert(ShapeComboBox.ItemIndex in [Ord(Low(PlannerItem.Shape))..Ord(High(PlannerItem.Shape))],
    'Shape index out of range');
end;

procedure TDefaultPlannerItemEditForm.CopyImageToSpeedButton(
  Index: Integer);
begin
  inherited;
  if Assigned(ImageList) and (Index >= 0) and (Index < ImageList.Count) then
  begin
    with ImageSpeedButton.Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end
  else // no ImageList or invalid Image
  begin
    ImageSpeedButton.Glyph := nil;
    //##jpl: To do - clear image of ImageSpeedButton
  end;
end;

procedure TDefaultPlannerItemEditForm.EditStateChanged;
begin
  inherited;
  if EditState = esNew then
  begin
    Caption := 'New appointment';
//      HTMLStaticText1.HTMLText.Add('  <IMG src="calendar">   <FONT face="Verdana">New appointment</FONT>');
  end
  else
  begin
    Caption := 'Edit appointment';
//      HTMLStaticText1.HTMLText.Add('  <IMG src="clock">   <FONT face="Verdana">Edit appointment</FONT>');
  end;
end;

procedure TDefaultPlannerItemEditForm.EndTimeDateTimePickerChange(
  Sender: TObject);
begin
  if EndTime < MinDuration then
    EndTime := MinDuration;
  if StartTime + MinDuration >= EndTime then
    StartTime := EndTime - MinDuration;

  ProcessWarnings;
end;

procedure TDefaultPlannerItemEditForm.FixEnabledComponents;
begin
  inherited;
  ReminderDateTimePicker.Enabled := chk_Reminder.Checked;
  ReminderMesssageEdit.Enabled := chk_Reminder.Checked;
  if Assigned(ImageList) then
  begin
    ImageSpeedButton.Enabled := True;
    ImageUpDown.Enabled := True;
    ImageUpDown.Min := -1; // -1 is none
    ImageUpDown.Max := ImageList.Count-1; // 0..Count-1 are images
  end
  else
  begin
    ImageSpeedButton.Enabled := False;
    ImageUpDown.Enabled := False;
    ImageUpDown.Min := 0;
    ImageUpDown.Max := 0;
  end;
end;

procedure TDefaultPlannerItemEditForm.FontButtonClick(Sender: TObject);
begin
  FontDialog.Font := FontPanel.Font;
  if FontDialog.Execute then
    FontPanel.Font := FontDialog.Font;
end;

procedure TDefaultPlannerItemEditForm.FormCreate(Sender: TObject);
const
  L_time_format = 'HH:mm:ss';    // 24-hour clock, hours, minutes & seconds
var
  fmt: string;
begin
  fmt := L_time_format;
  SendMessage(ReminderDateTimePicker.Handle, DTM_SETFORMATA, 0, LParam(fmt));
end;


function TDefaultPlannerItemEditForm.GetEndTime: TTime;
begin
  Result := Frac(EndTimeDateTimePicker.Time); // only time-portion
end;

function TDefaultPlannerItemEditForm.GetPlanDate: TDateTime;
begin
  Result := Trunc(PlanDateDateTimePicker.Date); // only date-portion
end;
function TDefaultPlannerItemEditForm.GetReminderTime: TTime;
begin
  Result := Frac(ReminderDateTimePicker.Time); // only time-portion
end;


function TDefaultPlannerItemEditForm.GetStartTime: TTime;
begin
  Result := Frac(StartTimeDateTimePicker.Time); // only time-portion
end;


procedure TDefaultPlannerItemEditForm.ImageChanged;
begin
  CopyImageToSpeedButton(ImageUpDown.Position);
end;

procedure TDefaultPlannerItemEditForm.ImageUpDownClick(Sender: TObject;
  Button: TUDBtnType);
begin
  ImageChanged;
end;

procedure TDefaultPlannerItemEditForm.Label6Click(Sender: TObject);
begin
  ImageUpDown.Position :=  ImageUpDown.Position + 1;
  CopyImageToSpeedButton(ImageUpDown.Position);
end;

procedure TDefaultPlannerItemEditForm.PlanDateDateTimePickerChange(
  Sender: TObject);
begin
  ProcessWarnings;
end;

procedure TDefaultPlannerItemEditForm.ProcessWarnings;
begin
  inherited;
  if (PlanDateDateTimePicker.Date < Date) or
     ((PlanDateDateTimePicker.Date = Date) and (StartTimeDateTimePicker.Time < Time))
  then
  begin
//  HTMLStaticText1.HTMLText.Clear;
//  HTMLStaticText1.HTMLText.Add('  <IMG src="caution">   <FONT face="Verdana">' +
//    'Warning: this appointment occurs in the past</FONT>');
    WarningPanel.Visible := True;
  end
  else
    WarningPanel.Visible := False;
end;

procedure TDefaultPlannerItemEditForm.chk_reminderClick(
  Sender: TObject);
begin
  FixEnabledComponents;
end;

procedure TDefaultPlannerItemEditForm.SetEndTime(const Value: TTime);
begin
  //TDateTimePicker has a bug where Date=0 and Time=0 gives an error
  EndTimeDateTimePicker.Date := 1; // work around a bug
  EndTimeDateTimePicker.Time := Frac(Value); // time only
end;

procedure TDefaultPlannerItemEditForm.SetImageList(
  const Value: TImageList);
begin
  if Value <> FImageList then
  begin
    FImageList := Value;
    FixEnabledComponents;
  end;
end;

procedure TDefaultPlannerItemEditForm.SetPlanDate(const Value: TDateTime);
begin
  PlanDateDateTimePicker.Date := Trunc(Value); // date only
end;

procedure TDefaultPlannerItemEditForm.SetReminderTime(const Value: TTime);
begin
  // TDateTimePicker has a bug where Date=0 and Time=0 gives an error
  ReminderDateTimePicker.Date := 1; // work around a bug
  ReminderDateTimePicker.Time := Frac(Value); // only time-portion
end;

procedure TDefaultPlannerItemEditForm.SetStartTime(const Value: TTime);
begin
  // TDateTimePicker has a bug where Date=0 and Time=0 gives an error
  StartTimeDateTimePicker.Date := 1; // work around a bug
  StartTimeDateTimePicker.Time := Frac(Value); // time only
end;


procedure TDefaultPlannerItemEditForm.SpeedButton1Click(Sender: TObject);
begin
  PlannerRecurrencyEditor1.Recurrency := Recurrency;
  if PlannerRecurrencyEditor1.Execute then
    Recurrency := PlannerRecurrencyEditor1.Recurrency;
end;

procedure TDefaultPlannerItemEditForm.btnLeftClick(Sender: TObject);
begin
  NotesMemo.Alignment := taLeftJustify;
end;

procedure TDefaultPlannerItemEditForm.btnCenterClick(Sender: TObject);
begin
  NotesMemo.Alignment := taCenter;
end;

procedure TDefaultPlannerItemEditForm.btnRightClick(Sender: TObject);
begin
  NotesMemo.Alignment := taRightJustify;
end;




procedure TDefaultPlannerItemEditForm.StartTimeDateTimePickerChange(
  Sender: TObject);
begin
  if StartTime > 1-MinDuration then
    StartTime := 1-MinDuration;
  if StartTime + MinDuration >= EndTime then
    EndTime := StartTime + MinDuration;

  ProcessWarnings;
end;

{ TDefaultItemEditor }

constructor TDefaultItemEditor.Create(AOwner: TComponent);
begin
  inherited;

  FRecurrencyDialog := TRecurrencyDialogLanguage.Create;
  FCenter := True;

  FLblWarning := 'Warning: this appointment occurs in the past';
  FLblSubject := '&Subject';
  FLblDate := '&Date';
  FLblFrom := '&From';
  FLblTo := '&To';
  FLblColor := '&Color';
  FLblShape := 'S&hape';
  FLblNotes := '&Notes';

  FLblFlashing := 'F&lashing';
  FLblReminder := '&Reminder';
  FLblMessage := '&Message';
  FLblURL := '&URL';
  FLblAddress := '&Address';
  FLblBackground := '&Background';
  FLblTrackbar := 'T&rackbar';
  FlblSelectedBackground := 'Selected bac&kground';
  FlblSelectedFont := 'S&elected font';
end;

procedure TDefaultItemEditor.CreateEditor(AOwner: TComponent);
begin
  inherited;

  FEditForm := TDefaultPlannerItemEditForm.Create(AOwner);
  FEditForm.Top := FFormTop;
  FEditForm.Left := FFormLeft;

  FEditForm.WarningPanel.Caption := FLblWarning;

  FEditForm.lbl_subject.Caption := FLblSubject;
  FEditForm.lbl_from.Caption := FLblFrom;
  FEditForm.lbl_date.Caption := FLblDate;
  FEditForm.lbl_notes.Caption := FLblNotes;
  FEditForm.lbl_to.Caption := FLblTo;
  FEditForm.lbl_shape.Caption := FLblShape;
  FEditForm.lbl_colors.Caption := FLblColor;

  FEditForm.chk_reminder.Caption := FLblReminder;
  FEditForm.chk_flashing.Caption := FLblFlashing;
  FEditForm.lbl_url.Caption := FLblURL;
  FEditForm.lbl_address.Caption := FLblAddress;
  FEditForm.lbl_msg.Caption := FLblMessage;
  FEditForm.lbl_address.Caption := FLblAddress;

  FEditForm.lbl_Background.Caption := FLblBackground;
  FEditForm.lbl_Trackbar.Caption := FLblTrackbar;
  FEditForm.lbl_selectedbackground.Caption := FLblSelectedBackground;
  FEditForm.lbl_selectedfont.Caption := FLblSelectedFont;
  FEditForm.PlannerRecurrencyEditor1.LanguageSettings.Assign(FRecurrencyDialog);

  if CenterOnScreen then
    FEditForm.Position := poScreenCenter;
  FEditForm.Caption := Caption;
end;

destructor TDefaultItemEditor.Destroy;
begin
  FRecurrencyDialog.Free;
  inherited;
end;

procedure TDefaultItemEditor.DestroyEditor;
begin
  ModalResult := FEditForm.ModalResult;
  inherited;
  FEditForm.Free;
end;

procedure TDefaultItemEditor.EditToPlannerItem(APlannerItem: TPlannerItem);
begin
  inherited;
  FEditForm.AssignToPlannerItem(APlannerItem);
end;

function TDefaultItemEditor.Execute: Integer;
begin
  Result := FEditForm.ShowModal;
end;

procedure TDefaultItemEditor.PlannerItemToEdit(APlannerItem: TPlannerItem);
begin
  inherited;
  FEditForm.AssignFromPlannerItem(APlannerItem);
end;

procedure TDefaultItemEditor.SetRecurencyDialog(
  const Value: TRecurrencyDialogLanguage);
begin
  FRecurrencyDialog.Assign(Value);
end;

end.
