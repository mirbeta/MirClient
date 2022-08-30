{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonFormWizard;

{$I cxVer.inc}

{$R dxRibbonFormWizard.res}

interface

uses
  Classes, Forms, Controls, StdCtrls, ExtCtrls, Menus,
  dxRibbonSkins, dxForms, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxPC, cxButtons,
  cxContainer, cxEdit, cxImage;

const
  dxRibbonFormWizardResType = 'DXWIZARDTEMPLATES';
  dxRibbonStyleNamePrefixMap: array[TdxRibbonStyle] of string = (
    '2007', '2010', '2013', '2016', '2016Tablet', '2019'
  );
  dxRibbonStyleDisplayNameMap: array[TdxRibbonStyle] of string = (
    'Office 2007', 'Office 2010', 'Office 2013', 'Office 2016', 'Office 2016 Tablet', 'Office 2019'
  );

type

  { TfrmRibbonFormWizard }

  TfrmRibbonFormWizard = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    imPreview: TcxImage;
    lbDescription: TLabel;
    tcRibbonStyle: TcxTabControl;

    procedure FormCreate(Sender: TObject);
    procedure tcRibbonStyleChange(Sender: TObject);
  strict private
    function GetSelectedRibbonStyle: TdxRibbonStyle;
    procedure SetSelectedRibbonStyle(const Value: TdxRibbonStyle);
  public
    property SelectedRibbonStyle: TdxRibbonStyle read GetSelectedRibbonStyle write SetSelectedRibbonStyle;
  end;

function ExecuteSelectRibbonStyleDialog(var ARibbonStyle: TdxRibbonStyle; const ACaption, APrompt: string): Boolean;
implementation

uses
  SysUtils, dxCore, dxGDIPlusClasses;

{$R *.dfm}

function ExecuteSelectRibbonStyleDialog(var ARibbonStyle: TdxRibbonStyle; const ACaption, APrompt: string): Boolean;
var
  ADialog: TfrmRibbonFormWizard;
begin
  ADialog := TfrmRibbonFormWizard.Create(nil);
  try
    ADialog.Caption := ACaption;
    ADialog.lbDescription.Caption := APrompt;
    ADialog.SelectedRibbonStyle := ARibbonStyle;
    Result := ADialog.ShowModal = mrOk;
    if Result then
      ARibbonStyle := ADialog.SelectedRibbonStyle;
  finally
    ADialog.Free;
  end;
end;

{ TfrmRibbonFormWizard }

procedure TfrmRibbonFormWizard.FormCreate(Sender: TObject);
var
  AStyle: TdxRibbonStyle;
begin
  tcRibbonStyle.Properties.Tabs.BeginUpdate;
  try
    tcRibbonStyle.Properties.Tabs.Clear;
    for AStyle := Low(TdxRibbonStyle) to High(TdxRibbonStyle) do
      tcRibbonStyle.Properties.Tabs.Add(dxRibbonStyleDisplayNameMap[AStyle]);
    SelectedRibbonStyle := High(AStyle);
  finally
    tcRibbonStyle.Properties.Tabs.EndUpdate;
  end;
end;

function TfrmRibbonFormWizard.GetSelectedRibbonStyle: TdxRibbonStyle;
begin
  Result := TdxRibbonStyle(tcRibbonStyle.TabIndex)
end;

procedure TfrmRibbonFormWizard.SetSelectedRibbonStyle(const Value: TdxRibbonStyle);
begin
  tcRibbonStyle.TabIndex := Ord(Value);
end;

procedure TfrmRibbonFormWizard.tcRibbonStyleChange(Sender: TObject);
var
  AImage: TdxSmartImage;
begin
  if tcRibbonStyle.TabIndex >= 0 then
  begin
    AImage := TdxSmartImage.Create;
    try
      AImage.LoadFromResource(HInstance, 'RIBBON' +
        dxRibbonStyleNamePrefixMap[SelectedRibbonStyle] + 'PREVIEW',
        dxRibbonFormWizardResType);
      imPreview.Picture.Graphic := AImage;
    finally
      AImage.Free;
    end;
  end;
end;

end.
