{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxSchedulerHolidaysLocationHolidayEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxTextEdit, cxControls, cxContainer, cxEdit, cxLabel, cxCheckBox,
  Menus, cxLookAndFeelPainters, StdCtrls, cxButtons,
  cxSchedulerHolidays, cxLookAndFeels, cxMaskEdit, cxDropDownEdit,
  cxCalendar, cxGraphics, ComCtrls, dxCore, cxDateUtils, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutContainer, dxLayoutLookAndFeels, cxClasses, dxLayoutControl, dxForms;

type
  TfmHolidaysLocationHolidayEditor = class(TdxForm)
    teName: TcxTextEdit;
    btnOk: TcxButton;
    btnCancel: TcxButton;
    deDate: TcxDateEdit;
    btnRecurrence: TcxButton;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    lbName: TdxLayoutItem;
    lbDate: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    procedure btnOkClick(Sender: TObject);
    procedure ValueChange(Sender: TObject);
    procedure btnRecurrenceClick(Sender: TObject);
  private
    FLocationHoliday: TcxSchedulerHolidaysLocationHoliday;
    procedure SetLocationHoliday(AValue: TcxSchedulerHolidaysLocationHoliday);
  protected
    procedure CheckButtonsState;
    procedure InitControls; virtual;
    function Save: Boolean; virtual;
    procedure SetCaptions;
  public
    function ShowModal: Integer; override;

    property LocationHoliday: TcxSchedulerHolidaysLocationHoliday read
      FLocationHoliday write SetLocationHoliday;
  end;

implementation

uses
  cxSchedulerStrs, cxSchedulerDialogs, DateUtils, cxSchedulerUtils;

{$R *.dfm}

type
  TcxDateEditAccess = class(TcxDateEdit);

{ TfmHolidaysLocationHolidayEditor }

function TfmHolidaysLocationHolidayEditor.ShowModal: Integer;
begin
  cxDialogsMetricsStore.InitDialog(Self);
  InitControls;
  CheckButtonsState;
  Result := inherited ShowModal;
  cxDialogsMetricsStore.StoreMetrics(Self);
end;

procedure TfmHolidaysLocationHolidayEditor.btnRecurrenceClick(Sender: TObject);
var
  AModified: Boolean;
  AStart: TDateTime;
begin
  AModified := False;
  AStart := LocationHoliday.Date;
  LocationHoliday.Date := deDate.Date;
  if cxShowRecurrenceHolidayEditor(LocationHoliday, teName.Style.LookAndFeel,
    AModified, False, True, Self) then
    deDate.Date := LocationHoliday.Date
  else
    LocationHoliday.Date := AStart;
  btnOk.Enabled := (teName.Text <> '') and (AModified or btnOk.Enabled);
end;

procedure TfmHolidaysLocationHolidayEditor.CheckButtonsState;
begin
  btnOk.Enabled := (Length(teName.Text) > 0) and
    (TcxDateEditAccess(deDate).DisplayText <> '') and (deDate.Date <> NullDate) and
    (LocationHoliday.Location.Find(teName.Text, deDate.Date) = nil);
end;

procedure TfmHolidaysLocationHolidayEditor.InitControls;
begin
  if UseSchedulerColorInDialogs then
    Color := btnCancel.LookAndFeel.Painter.DefaultSchedulerControlColor;
  SetCaptions;
  teName.Text := FLocationHoliday.Name;
  deDate.Date := FLocationHoliday.Date;
  CheckButtonsState;
end;

function TfmHolidaysLocationHolidayEditor.Save: Boolean;
begin
  Result := True;
  with FLocationHoliday do
  begin
    Name := teName.Text;
    Date := deDate.Date;
  end;
end;

procedure TfmHolidaysLocationHolidayEditor.SetCaptions;
begin
  Caption := cxGetResourceString(@scxHolidaysLocationHolidayEditorCaption);
  btnCancel.Caption := cxGetResourceString(@scxCancel);
  btnOk.Caption := cxGetResourceString(@scxOk);
  btnRecurrence.Caption := cxGetResourceString(@scxRecurrence);

  lbName.Caption := cxGetResourceString(@scxHolidayName);
  lbDate.Caption := cxGetResourceString(@scxHolidayDate);
end;

procedure TfmHolidaysLocationHolidayEditor.SetLocationHoliday(
  AValue: TcxSchedulerHolidaysLocationHoliday);
begin
  FLocationHoliday := AValue;
end;

procedure TfmHolidaysLocationHolidayEditor.btnOkClick(Sender: TObject);
begin
  Save;
end;

procedure TfmHolidaysLocationHolidayEditor.ValueChange(Sender: TObject);
begin
  CheckButtonsState;
end;

end.
