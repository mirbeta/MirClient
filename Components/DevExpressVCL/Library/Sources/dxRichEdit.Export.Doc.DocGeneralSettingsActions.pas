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
unit dxRichEdit.Export.Doc.DocGeneralSettingsActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.PieceTable;

type

  { TdxDocGeneralSettingsActions }

  TdxDocGeneralSettingsActions = class
  strict private
    FWriter: TBinaryWriter;
    FGeneralSettings: TdxSectionGeneralSettings;
  public
    constructor Create(AOutput: TdxMemoryStream; AGeneralSettings: TdxSectionGeneralSettings);
    destructor Destroy; override;
    procedure DifferentFirstPageAction;
    procedure FirstPagePaperSourceAction;
    procedure OnlyAllowEditingOfFormFieldsAction;
    procedure OtherPagePaperSourceAction;
    procedure StartTypeAction;
    procedure TextDirectionAction;
    procedure VerticalTextAlignmentAction;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocGeneralSettingsActions }

constructor TdxDocGeneralSettingsActions.Create(AOutput: TdxMemoryStream; AGeneralSettings: TdxSectionGeneralSettings);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FGeneralSettings := AGeneralSettings;
end;

destructor TdxDocGeneralSettingsActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocGeneralSettingsActions.DifferentFirstPageAction;
var
  ACommand: TdxDocCommandDifferentFirstPage;
begin
  ACommand := TdxDocCommandDifferentFirstPage.Create;
  try
    ACommand.Value := FGeneralSettings.DifferentFirstPage;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocGeneralSettingsActions.FirstPagePaperSourceAction;
var
  ACommand: TdxDocCommandFirstPagePaperSource;
begin
  ACommand := TdxDocCommandFirstPagePaperSource.Create;
  try
    ACommand.Value := FGeneralSettings.FirstPagePaperSource;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocGeneralSettingsActions.OnlyAllowEditingOfFormFieldsAction;
var
  ACommand: TdxDocCommandOnlyAllowEditingOfFormFields;
begin
  ACommand := TdxDocCommandOnlyAllowEditingOfFormFields.Create;
  try
    ACommand.Value := FGeneralSettings.OnlyAllowEditingOfFormFields;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocGeneralSettingsActions.OtherPagePaperSourceAction;
var
  ACommand: TdxDocCommandOtherPagePaperSource;
begin
  ACommand := TdxDocCommandOtherPagePaperSource.Create;
  try
    ACommand.Value := FGeneralSettings.OtherPagePaperSource;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocGeneralSettingsActions.StartTypeAction;
var
  ACommand: TdxDocCommandStartType;
begin
  ACommand := TdxDocCommandStartType.Create;
  try
    ACommand.StartType := FGeneralSettings.StartType;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocGeneralSettingsActions.TextDirectionAction;
var
  ACommand: TdxDocCommandTextDirection;
begin
  ACommand := TdxDocCommandTextDirection.Create;
  try
    ACommand.TextDirection := FGeneralSettings.TextDirection;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocGeneralSettingsActions.VerticalTextAlignmentAction;
var
  ACommand: TdxDocCommandVerticalTextAlignment;
begin
  ACommand := TdxDocCommandVerticalTextAlignment.Create;
  try
    ACommand.VerticalTextAlignment := FGeneralSettings.VerticalTextAlignment;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

end.
