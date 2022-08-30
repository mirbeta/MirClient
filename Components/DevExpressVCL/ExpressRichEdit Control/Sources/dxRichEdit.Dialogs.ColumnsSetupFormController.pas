{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.ColumnsSetupFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface
uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.UnitConverter;

type

  { TdxColumnsSetupFormControllerParameters }

  TdxColumnsSetupFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FColumnsInfo: TdxColumnsInfoUI;
  public
    constructor Create(const AControl: IdxRichEditControl; AColumnsInfo: TdxColumnsInfoUI);
    property ColumnsInfo: TdxColumnsInfoUI read FColumnsInfo;
  end;

  { TdxColumnsSetupFormController }

  TdxColumnsSetupFormController = class(TdxFormController)
  strict private
    FControl: IdxRichEditControl;
    FColumnsInfo: TdxColumnsInfoUI;
    FSourceColumnsInfo: TdxColumnsInfoUI;
    FValueUnitConverter: TdxDocumentModelUnitConverter;
    function GetAvailableApplyTypes: TdxSectionPropertiesApplyTypes;
    function GetApplyType: TdxSectionPropertiesApplyType;
    procedure SetApplyType(const Value: TdxSectionPropertiesApplyType);
  public
    constructor Create(AControllerParameters: TdxColumnsSetupFormControllerParameters);
    destructor Destroy; override;
    procedure ApplyChanges; override;
    procedure ChangeColumnCount(ACount: Integer);
    procedure SetEqualColumnWidth(AValue: Boolean);
    procedure ApplyPreset(APreset: TdxColumnsInfoPreset);

    property ValueUnitConverter: TdxDocumentModelUnitConverter read FValueUnitConverter write FValueUnitConverter;
    property SourceColumnsInfo: TdxColumnsInfoUI read FSourceColumnsInfo;
    property ColumnsInfo: TdxColumnsInfoUI read FColumnsInfo;
    property AvailableApplyType: TdxSectionPropertiesApplyTypes read GetAvailableApplyTypes;
    property ApplyType: TdxSectionPropertiesApplyType read GetApplyType write SetApplyType;
  end;

implementation

{ TdxColumnsSetupFormControllerParameters }

constructor TdxColumnsSetupFormControllerParameters.Create(const AControl: IdxRichEditControl; AColumnsInfo: TdxColumnsInfoUI);
begin
  inherited Create(AControl);
  FColumnsInfo := AColumnsInfo;
end;

{ TdxColumnsSetupFormController }

constructor TdxColumnsSetupFormController.Create(AControllerParameters: TdxColumnsSetupFormControllerParameters);
begin
  inherited Create;
  FSourceColumnsInfo := AControllerParameters.ColumnsInfo;
  FColumnsInfo := FSourceColumnsInfo.Clone;
  FControl := AControllerParameters.Control;
  FValueUnitConverter := FControl.InnerControl.DocumentModel.UnitConverter;
end;

destructor TdxColumnsSetupFormController.Destroy;
begin
  FColumnsInfo.Free;
  inherited Destroy;
end;

function TdxColumnsSetupFormController.GetAvailableApplyTypes: TdxSectionPropertiesApplyTypes;
begin
  Result := FColumnsInfo.AvailableApplyType;
end;

function TdxColumnsSetupFormController.GetApplyType: TdxSectionPropertiesApplyType;
begin
  Result := FColumnsInfo.ApplyType;
end;

procedure TdxColumnsSetupFormController.ApplyChanges;
begin
  FSourceColumnsInfo.CopyFrom(FColumnsInfo);
end;

procedure TdxColumnsSetupFormController.ChangeColumnCount(ACount: Integer);
begin
  FColumnsInfo.ChangeColumnCount(ACount);
end;

procedure TdxColumnsSetupFormController.SetApplyType(const Value: TdxSectionPropertiesApplyType);
begin
  FColumnsInfo.ApplyType := Value;
end;

procedure TdxColumnsSetupFormController.SetEqualColumnWidth(AValue: Boolean);
begin
  FColumnsInfo.EqualColumnWidth := AValue;
  if AValue then
    FColumnsInfo.CalculateUniformColumnsByColumnSpacing;
end;

procedure TdxColumnsSetupFormController.ApplyPreset(APreset: TdxColumnsInfoPreset);
begin
  APreset.ApplyTo(ColumnsInfo);
end;

end.
