{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulerRecurrenceSelectionDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Menus,
  dxCore, cxLookAndFeelPainters, cxButtons, cxRadioGroup, cxSchedulerStorage, cxGraphics, cxLookAndFeels,
  cxControls, cxContainer, cxEdit, cxLabel, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, dxForms;

type
  { TfmRecurrenceSelection }

  TcxRecurrenceSelectionMode = (rsmOpen, rsmDeleting);

  TfmRecurrenceSelectionForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    Image: TImage;
    lbMessage: TdxLayoutLabeledItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    rbOccurrence: TcxRadioButton;
    rbSeries: TcxRadioButton;
  private
    FEvent: TcxSchedulerEvent;
    FMode: TcxRecurrenceSelectionMode;
  protected
    procedure DoShow; override;
    function GetFormColor: TColor; virtual;
    procedure SetCaptions; virtual;

    property Event: TcxSchedulerEvent read FEvent;
    property Mode: TcxRecurrenceSelectionMode read FMode;
  public
    constructor CreateEx(AEvent: TcxSchedulerEvent; AMode: TcxRecurrenceSelectionMode); virtual;
  end;

implementation

uses
  cxSchedulerStrs, cxSchedulerDialogs;

{$R *.dfm}

{ TfmRecurrenceSelection }

constructor TfmRecurrenceSelectionForm.CreateEx(AEvent: TcxSchedulerEvent; AMode: TcxRecurrenceSelectionMode);
begin
  Create(Application);
  FEvent := AEvent;
  FMode := AMode;
  Image.Picture.Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);
  SetCaptions;
end;

procedure TfmRecurrenceSelectionForm.DoShow;
begin
  if UseSchedulerColorInDialogs then
    Color := GetFormColor;
end;

function TfmRecurrenceSelectionForm.GetFormColor: TColor;
begin
  Result := btnOk.LookAndFeel.Painter.DefaultSchedulerControlColor;
end;

procedure TfmRecurrenceSelectionForm.SetCaptions;
var
  AEventCaption: string;
begin
  AEventCaption := '"' + Event.Caption + '" ';
  if Mode = rsmOpen then
  begin
    Caption := cxGetResourceString(@scxEditTypeDialogCaption);
    lbMessage.Caption := AEventCaption +
      cxGetResourceString(@scxEditRecurringEventDescription);
    rbOccurrence.Caption := cxGetResourceString(@scxEditTypeOccurrenceLabel);
    rbSeries.Caption := cxGetResourceString(@scxEditTypeSeriesLabel);
  end
  else
  begin
    Caption := cxGetResourceString(@scxDeleteTypeDialogCaption);
    lbMessage.Caption := AEventCaption +
      cxGetResourceString(@scxDeleteRecurringEventDescription);
    rbOccurrence.Caption := cxGetResourceString(@scxDeleteTypeOccurrenceLabel);
    rbSeries.Caption := cxGetResourceString(@scxDeleteTypeSeriesLabel);
  end;
  btnOk.Caption := cxGetResourcestring(@scxOk);
  btnCancel.Caption := cxGetResourcestring(@scxCancel);
end;

end.
