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

unit PlanPeriodEdit;

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
  TPeriodPlannerItemEditForm = class(TForm)
    lbl_subject: TLabel;
    lbl_start: TLabel;
    lbl_notes: TLabel;
    lbl_shape: TLabel;
    lbl_color: TLabel;
    StartTime: TDateTimePicker;
    EndTime: TDateTimePicker;
    CBShape: TComboBox;
    EdSubject: TEdit;
    Notes: TMemo;
    StartDate: TDateTimePicker;
    ColorDialog: TColorDialog;
    Panel2: TPanel;
    ButtonBottomPanel: TPanel;
    ButtonBottomRightPanel: TPanel;
    OKBtn: TButton;
    CancBtn: TButton;
    WarningPanel: TPanel;
    EndDate: TDateTimePicker;
    lbl_end: TLabel;
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

  TPeriodItemEditor = class(TCustomItemEditor)
  private
    FEditForm: TPeriodPlannerItemEditForm;
    FCenter: Boolean;
    FFormLeft: Integer;
    FFormTop: Integer;
    FLblSubject: string;
    FLblWarning: string;
    FShowShape: boolean;
    FShowColor: boolean;
    FLblEnd: string;
    FLblNotes: string;
    FLblShape: string;
    FLblColor: string;
    FLblStart: string;
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
    property LblStart: string read FLblStart write FLblStart;
    property LblEnd: string read FLblEnd write FLblEnd;
    property LblColor: string read FLblColor write FLblColor;
    property LblShape: string read FLblShape write FLblShape;
    property LblNotes: string read FLblNotes write FLblNotes;
    property ShowShape: boolean read FShowShape write FShowShape default true;
    property ShowColor: boolean read FShowColor write FShowColor default true;
  end;


implementation

{$R *.dfm}


{ TDefaultPlannerItemEditForm }

procedure TPeriodPlannerItemEditForm.EditModal(PlannerItem: TPlannerItem);
begin
  Assert(Assigned(PlannerItem), 'Must pass a non-nil planner item for editing');
  EditState := esEdit;
  InternalEditModal(PlannerItem);
end;

procedure TPeriodPlannerItemEditForm.InternalEditModal(
  PlannerItem: TPlannerItem);
begin
  AssignFromPlannerItem(PlannerItem);
  if (ShowModal = mrOK) then
    AssignToPlannerItem(PlannerItem);
end;

procedure TPeriodPlannerItemEditForm.SetEditState(
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

procedure TPeriodPlannerItemEditForm.Shape1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := Shape1.Brush.Color;
  if ColorDialog.Execute then
    Shape1.Brush.Color := ColorDialog.Color;
end;

procedure TPeriodPlannerItemEditForm.AssignFromPlannerItem(
  PlannerItem: TPlannerItem);
begin
  EdSubject.Text := PlannerItem.CaptionText;
  StartDate.Date := PlannerItem.ItemRealStartTime;
  StartTime.Time := PlannerItem.ItemRealStartTime;
  EndDate.Date := PlannerItem.ItemRealEndTime;
  EndTime.Time := PlannerItem.ItemRealEndTime;

  Notes.Lines := PlannerItem.Text;
  Shape1.Brush.Color := PlannerItem.Color;
  CBShape.ItemIndex := Ord(PlannerItem.Shape);
  ProcessWarnings;
end;

procedure TPeriodPlannerItemEditForm.AssignToPlannerItem(
  PlannerItem: TPlannerItem);
begin
  PlannerItem.CaptionText := EdSubject.Text;

  with PlannerItem.ParentItem do
  begin
    ItemStartTime := Int(StartDate.Date) + Frac(StartTime.Time);
    ItemEndTime := Int(EndDate.Date) + Frac(EndTime.Time);

    ItemRealStartTime := Int(StartDate.Date) + Frac(StartTime.Time);
    ItemRealEndTime := Int(EndDate.Date) + Frac(EndTime.Time);

    RealTime := True;

    Text := TStringList(Notes.Lines);

    Color := Shape1.Brush.Color;

    Shape := TPlannerShape(CBShape.ItemIndex);
    Update;
  end;

end;

procedure TPeriodPlannerItemEditForm.ProcessWarnings;
begin
  if (StartDate.Date < Date) or ((StartDate.Date = Date) and (StartTime.Time < Time)) then
  begin
    WarningPanel.Visible := True;
  end
  else
    WarningPanel.Visible := False;
end;

{ TPeriodItemEditor }

constructor TPeriodItemEditor.Create(AOwner: TComponent);
begin
  inherited;
  FCenter := True;

  FLblWarning := 'Warning: this appointment occurs in the past';
  FLblSubject := '&Subject';
  FLblStart := 'S&tart time';
  FLblEnd := '&End time';
  FLblColor := '&Color';
  FLblShape := 'S&hape';
  FLblNotes := '&Notes';
end;

procedure TPeriodItemEditor.CreateEditor(AOwner: TComponent);
begin
  inherited;
  FEditForm := TPeriodPlannerItemEditForm.Create(AOwner);
  FEditForm.Top := FFormTop;
  FEditForm.Left := FFormLeft;

  FEditForm.WarningPanel.Caption := FLblWarning;
  FEditForm.lbl_subject.Caption := FLblSubject;
  FEditForm.lbl_start.Caption := FLblStart;
  FEditForm.lbl_notes.Caption := FLblNotes;
  FEditForm.lbl_end.Caption := FLblEnd;
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

procedure TPeriodItemEditor.DestroyEditor;
begin
  ModalResult := FEditForm.ModalResult;
  inherited;
  FEditForm.Free;
end;

procedure TPeriodItemEditor.EditToPlannerItem(APlannerItem: TPlannerItem);
begin
  inherited;
  FEditForm.AssignToPlannerItem(APlannerItem);
end;

function TPeriodItemEditor.Execute: Integer;
begin
  Result := FEditForm.ShowModal;
end;

procedure TPeriodItemEditor.PlannerItemToEdit(APlannerItem: TPlannerItem);
begin
  inherited;
  FEditForm.AssignFromPlannerItem(APlannerItem);
end;

end.
