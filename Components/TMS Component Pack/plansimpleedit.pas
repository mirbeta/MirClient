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

unit PlanSimpleEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Planner, StdCtrls, ExtCtrls, ComCtrls
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TEditState = (esEdit, esNew);

type
  TSimplePlannerItemEditForm = class(TForm)
    lbl_subj: TLabel;
    lbl_from: TLabel;
    lbl_date: TLabel;
    lbl_notes: TLabel;
    lbl_to: TLabel;
    lbl_shape: TLabel;
    lbl_color: TLabel;
    StartTime: TDateTimePicker;
    EndTime: TDateTimePicker;
    CBShape: TComboBox;
    EdSubject: TEdit;
    Notes: TMemo;
    PlanDate: TDateTimePicker;
    ColorDialog: TColorDialog;
    WarningPanel: TPanel;
    ButtonBottomPanel: TPanel;
    ButtonBottomRightPanel: TPanel;
    OKBtn: TButton;
    CancBtn: TButton;
    Panel1: TPanel;
    Shape1: TShape;
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FEditState: TEditState;
  protected
    procedure AssignFromPlannerItem(PlannerItem: TPlannerItem); virtual;
    procedure AssignToPlannerItem(PlannerItem: TPlannerItem); virtual;
    procedure InternalEditModal(PlannerItem: TPlannerItem); virtual;
    procedure ProcessWarnings; virtual;
    procedure SetEditState(const Value: TEditState); virtual;
  public
    procedure EditModal(PlannerItem: TPlannerItem); virtual;
    property EditState: TEditState read FEditState write SetEditState;
  end;

  TSimpleItemEditor = class(TCustomItemEditor)
  private
    FEditForm: TSimplePlannerItemEditForm;
    FCenter: Boolean;
    FFormLeft: Integer;
    FFormTop: Integer;
    FLblWarning: string;
    FLblSubject: string;
    FLblDate: string;
    FLblFrom: string;
    FLblTo: string;
    FLblColor: string;
    FLblShape: string;
    FLblNotes: string;
    FShowShape: boolean;
    FShowColor: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateEditor(AOwner: TComponent); override;
    procedure DestroyEditor; override;
    function Execute: Integer; override;
    procedure PlannerItemToEdit(APlannerItem: TPlannerItem); override;
    procedure EditToPlannerItem(APlannerItem: TPlannerItem); override;
  published
    property CenterOnScreen: Boolean read FCenter write FCenter default True;
    property FormLeft: Integer read FFormLeft write FFormLeft;
    property FormTop: Integer read FFormTop write FFormTop;
    property LblWarning: string read FLblWarning write FLblWarning;
    property LblSubject: string read FLblSubject write FLblSubject;
    property LblDate: string read FLblDate write FLblDate;
    property LblFrom: string read FLblFrom write FLblFrom;
    property LblTo: string read FLblTo write FLblTo;
    property LblColor: string read FLblColor write FLblColor;
    property LblShape: string read FLblShape write FLblShape;
    property LblNotes: string read FLblNotes write FLblNotes;
    property ShowShape: boolean read FShowShape write FShowShape default true;
    property ShowColor: boolean read FShowColor write FShowColor default true;
  end;


implementation

{$R *.dfm}


{ TDefaultPlannerItemEditForm }

procedure TSimplePlannerItemEditForm.EditModal(PlannerItem: TPlannerItem);
begin
  Assert(Assigned(PlannerItem), 'Must pass a non-nil planner item for editing');
  EditState := esEdit;
  InternalEditModal(PlannerItem);
end;

procedure TSimplePlannerItemEditForm.InternalEditModal(
  PlannerItem: TPlannerItem);
begin
  AssignFromPlannerItem(PlannerItem);
  if (ShowModal = mrOK) then
    AssignToPlannerItem(PlannerItem);
end;

procedure TSimplePlannerItemEditForm.SetEditState(
  const Value: TEditState);
begin
  if Value <> EditState then
  begin
    FEditState := Value;
    if Value = esNew then
    begin
      Caption := 'New appointment';
    end
    else
    begin
      Caption := 'Edit appointment';
    end;
  end;
end;

procedure TSimplePlannerItemEditForm.Shape1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := Shape1.Brush.Color;
  if ColorDialog.Execute then
    Shape1.Brush.Color := ColorDialog.Color;

end;

procedure TSimplePlannerItemEditForm.AssignFromPlannerItem(
  PlannerItem: TPlannerItem);
begin
  EdSubject.Text := PlannerItem.CaptionText;

  if PlannerItem.RealTime then
  begin
    PlanDate.Date := PlannerItem.ItemRealStartTime;
    StartTime.Time := PlannerItem.ItemRealStartTime;
    EndTime.Time := PlannerItem.ItemRealEndTime;
  end
  else
  begin
    PlanDate.Date := PlannerItem.ItemStartTime;
    StartTime.Time := PlannerItem.ItemStartTime;
    EndTime.Time := PlannerItem.ItemEndTime;
  end;

  Notes.Lines := PlannerItem.Text;
  Shape1.Brush.Color       := PlannerItem.Color;
  CBShape.ItemIndex      := Ord(PlannerItem.Shape);
  ProcessWarnings;
end;

procedure TSimplePlannerItemEditForm.AssignToPlannerItem(
  PlannerItem: TPlannerItem);
begin
  PlannerItem.CaptionText := EdSubject.Text;

  PlannerItem.ItemStartTime := Int(PlanDate.Date) + Frac(StartTime.Time);
  PlannerItem.ItemEndTime := Int(PlanDate.Date) + Frac(EndTime.Time);

  PlannerItem.ItemRealStartTime := Int(PlanDate.Date) + Frac(StartTime.Time);
  PlannerItem.ItemRealEndTime := Int(PlanDate.Date) + Frac(EndTime.Time);

  PlannerItem.RealTime := True;

  PlannerItem.Text := TStringList(Notes.Lines);

  // PlannerItem.ImageID := ImagePicker.ItemIndex;
  PlannerItem.Color := Shape1.Brush.Color;
  PlannerItem.ColorTo := Shape1.Brush.Color;

  // three possible shapes: 1-psRect, 2-psRounded, 3-psHexagon
  Assert(CBShape.ItemIndex in [Ord(Low(PlannerItem.Shape))..Ord(High(PlannerItem.Shape))],
    'Shape index out of range');
  PlannerItem.Shape := TPlannerShape(CBShape.ItemIndex);

  PlannerItem.Update;
end;

procedure TSimplePlannerItemEditForm.ProcessWarnings;
begin
  if (PlanDate.Date < Date) or ((PlanDate.Date = Date) and (StartTime.Time < Time)) then
  begin
    WarningPanel.Visible := True;
  end
  else
    WarningPanel.Visible := False;
end;

{ TSimpleItemEditor }

constructor TSimpleItemEditor.Create(AOwner: TComponent);
begin
  inherited;
  FCenter := True;
  FLblWarning := 'Warning: this appointment occurs in the past';
  FLblSubject := '&Subject';
  FLblDate := '&Date';
  FLblFrom := '&From';
  FLblTo := '&To';
  FLblColor := '&Color';
  FLblShape := 'S&hape';
  FLblNotes := '&Notes';
  FShowShape := true;
  FShowColor := true;
end;

procedure TSimpleItemEditor.CreateEditor(AOwner: TComponent);
begin
  inherited;
  FEditForm := TSimplePlannerItemEditForm.Create(AOwner);
  FEditForm.Top := FFormTop;
  FEditForm.Left := FFormLeft;

  FEditForm.WarningPanel.Caption := FLblWarning;
  FEditForm.lbl_subj.Caption := FLblSubject;
  FEditForm.lbl_from.Caption := FLblFrom;
  FEditForm.lbl_date.Caption := FLblDate;
  FEditForm.lbl_notes.Caption := FLblNotes;
  FEditForm.lbl_to.Caption := FLblTo;
  FEditForm.lbl_shape.Caption := FLblShape;
  FEditForm.lbl_color.Caption := FLblColor;

  FEditForm.lbl_shape.Visible := FShowShape;
  FEditForm.CBShape.Visible := FShowShape;

  FEditForm.lbl_color.Visible := FShowColor;
  FEditForm.Shape1.Visible := FShowColor;

  if CenterOnScreen then
    FEditForm.Position := poScreenCenter;
  FEditForm.Caption := Caption;  
end;

procedure TSimpleItemEditor.DestroyEditor;
begin
  inherited;
  FEditForm.Free;
end;

procedure TSimpleItemEditor.EditToPlannerItem(APlannerItem: TPlannerItem);
begin
  inherited;
  FEditForm.AssignToPlannerItem(APlannerItem);
end;

function TSimpleItemEditor.Execute: Integer;
begin
  Result := FEditForm.ShowModal;
end;

procedure TSimpleItemEditor.PlannerItemToEdit(APlannerItem: TPlannerItem);
begin
  inherited;
  FEditForm.AssignFromPlannerItem(APlannerItem);
end;

end.
