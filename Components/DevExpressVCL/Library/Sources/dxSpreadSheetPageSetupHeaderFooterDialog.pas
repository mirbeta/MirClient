{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetPageSetupHeaderFooterDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFDEF DELPHI101BERLIN}
  System.ImageList,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, cxButtons, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel, cxRadioGroup,
  dxSpreadSheetTypes, dxSpreadSheetCore, dxForms, dxSpreadSheetPrinting, cxTextEdit, cxMemo, cxEditRepositoryItems,
  ImgList, cxImageList;

type

  { TdxSpreadSheetPageSetupHeaderFooterDialogForm }

  TdxSpreadSheetPageSetupHeaderFooterDialogFormClass = class of TdxSpreadSheetPageSetupHeaderFooterDialogForm;
  TdxSpreadSheetPageSetupHeaderFooterDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnInsertDate: TcxButton;
    btnInsertPageNumber: TcxButton;
    btnInsertPageTotal: TcxButton;
    btnInsertSheetName: TcxButton;
    btnInsertTime: TcxButton;
    btnOK: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem;
    dxLayoutEmptySpaceItem4: TdxLayoutEmptySpaceItem;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    EditRepository: TcxEditRepository;
    ermiSection: TcxEditRepositoryMemoItem;
    ilImages: TcxImageList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lgCommonHeaderFooter: TdxLayoutGroup;
    lgTabs: TdxLayoutGroup;
    liFooterCenter: TdxLayoutItem;
    liFooterLeft: TdxLayoutItem;
    liFooterRight: TdxLayoutItem;
    liHeaderCenter: TdxLayoutItem;
    liHeaderLeft: TdxLayoutItem;
    liHeaderRight: TdxLayoutItem;
    lliDescription: TdxLayoutLabeledItem;
    meFooterCenter: TcxMemo;
    meFooterLeft: TcxMemo;
    meFooterRight: TcxMemo;
    meHeaderCenter: TcxMemo;
    meHeaderLeft: TcxMemo;
    meHeaderRight: TcxMemo;

    procedure btnInsertSheetNameClick(Sender: TObject);
  protected
    FSheet: TdxSpreadSheetTableView;

    procedure ApplyLocalization; virtual;
    procedure Initialize(ASheet: TdxSpreadSheetTableView); virtual;
    procedure Load(AOptions: TdxSpreadSheetTableViewOptionsPrintHeaderFooter); virtual;
    procedure Save(AOptions: TdxSpreadSheetTableViewOptionsPrintHeaderFooter); virtual;
  end;

var
  SpreadSheetPageSetupHeaderFooterDialogClass: TdxSpreadSheetPageSetupHeaderFooterDialogFormClass = TdxSpreadSheetPageSetupHeaderFooterDialogForm;

function ShowPageHeaderFooterSetupDialog(ASheet: TdxSpreadSheetTableView;
  AOptions: TdxSpreadSheetTableViewOptionsPrintHeaderFooter): Boolean;
implementation

uses
  dxSpreadSheetDialogStrs;

{$R *.dfm}

function ShowPageHeaderFooterSetupDialog(ASheet: TdxSpreadSheetTableView;
  AOptions: TdxSpreadSheetTableViewOptionsPrintHeaderFooter): Boolean;
var
  ADialog: TdxSpreadSheetPageSetupHeaderFooterDialogForm;
begin
  ADialog := SpreadSheetPageSetupHeaderFooterDialogClass.Create(GetParentForm(ASheet.SpreadSheet));
  try
    ADialog.Initialize(ASheet);
    ADialog.Load(AOptions);
    Result := ADialog.ShowModal = mrOk;
    if Result then
      ADialog.Save(AOptions);
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetPageSetupHeaderFooterDialogForm }

procedure TdxSpreadSheetPageSetupHeaderFooterDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogCaption);
  btnOK.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogButtonCancel);

  // Common Header / Footer
  lgCommonHeaderFooter.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogCaption);
  btnInsertDate.Hint := cxGetResourceString(@sdxPageSetupHeaderFooterDialogHintInsertDate);
  btnInsertPageNumber.Hint := cxGetResourceString(@sdxPageSetupHeaderFooterDialogHintInsertPageNumber);
  btnInsertPageTotal.Hint := cxGetResourceString(@sdxPageSetupHeaderFooterDialogHintInsertPageTotal);
  btnInsertSheetName.Hint := cxGetResourceString(@sdxPageSetupHeaderFooterDialogHintInsertSheetName);
  btnInsertTime.Hint := cxGetResourceString(@sdxPageSetupHeaderFooterDialogHintInsertTime);
  liFooterCenter.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogCenterFooter);
  liFooterLeft.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogLeftFooter);
  liFooterRight.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogRightFooter);
  liHeaderCenter.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogCenterHeader);
  liHeaderLeft.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogLeftHeader);
  liHeaderRight.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogRightHeader);
  lliDescription.Caption := cxGetResourceString(@sdxPageSetupHeaderFooterDialogDescription);
end;

procedure TdxSpreadSheetPageSetupHeaderFooterDialogForm.btnInsertSheetNameClick(Sender: TObject);
var
  AControl: TControl;
  AMacro: string;
begin
  case TComponent(Sender).Tag of
    1: AMacro := '&P';
    2: AMacro := '&N';
    3: AMacro := '&D';
    4: AMacro := '&T';
    5: AMacro := '&A';
  end;

  if AMacro <> '' then
  begin
    AControl := FindControl(GetFocus);
    if AControl is TcxCustomInnerMemo then
      TcxCustomInnerMemo(AControl).SelText := AMacro
    else
    begin
      meHeaderCenter.SetFocus;
      meHeaderCenter.SelText := AMacro;
    end;
  end;
end;

procedure TdxSpreadSheetPageSetupHeaderFooterDialogForm.Initialize(ASheet: TdxSpreadSheetTableView);
begin
  FSheet := ASheet;
  SetControlLookAndFeel(Self, FSheet.SpreadSheet.DialogsLookAndFeel);
  ApplyLocalization;
end;

procedure TdxSpreadSheetPageSetupHeaderFooterDialogForm.Load(AOptions: TdxSpreadSheetTableViewOptionsPrintHeaderFooter);
begin
  meHeaderLeft.Text := AOptions.CommonHeader.LeftSection;
  meHeaderCenter.Text := AOptions.CommonHeader.CenterSection;
  meHeaderRight.Text := AOptions.CommonHeader.RightSection;

  meFooterLeft.Text := AOptions.CommonFooter.LeftSection;
  meFooterCenter.Text := AOptions.CommonFooter.CenterSection;
  meFooterRight.Text := AOptions.CommonFooter.RightSection;
end;

procedure TdxSpreadSheetPageSetupHeaderFooterDialogForm.Save(AOptions: TdxSpreadSheetTableViewOptionsPrintHeaderFooter);
begin
  AOptions.CommonHeader.LeftSection := meHeaderLeft.Text;
  AOptions.CommonHeader.CenterSection := meHeaderCenter.Text;
  AOptions.CommonHeader.RightSection := meHeaderRight.Text;

  AOptions.CommonFooter.LeftSection := meFooterLeft.Text;
  AOptions.CommonFooter.CenterSection := meFooterCenter.Text;
  AOptions.CommonFooter.RightSection := meFooterRight.Text;
end;

end.
