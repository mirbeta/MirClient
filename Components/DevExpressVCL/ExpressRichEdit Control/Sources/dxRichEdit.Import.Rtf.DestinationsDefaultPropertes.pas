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

unit dxRichEdit.Import.Rtf.DestinationsDefaultPropertes;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCoreClasses, dxRichEdit.Import.Rtf, dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxDefaultCharacterPropertiesDestination = class(TdxDestinationPieceTable)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  protected
    function CanAppendText: Boolean; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessCharCore(AChar: Char); override;
  public
    constructor Create(AImporter: TdxRtfImporter); reintroduce;
    procedure BeforePopRtfState; override;
    procedure FinalizePieceTableCreation; override;
  end;

  TdxDefaultParagraphPropertiesDestination = class(TdxDestinationPieceTable)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  protected
    function CanAppendText: Boolean; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessCharCore(AChar: Char); override;
  public
    constructor Create(AImporter: TdxRtfImporter); reintroduce;
    procedure BeforePopRtfState; override;
    procedure FinalizePieceTableCreation; override;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Core, dxRichEdit.Utils.BatchUpdateHelper, RTLConsts,
  dxRichEdit.Import.Rtf.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting, dxRichEdit.DocumentModel.ParagraphFormatting;

{ TdxDefaultCharacterPropertiesDestination }

procedure TdxDefaultCharacterPropertiesDestination.BeforePopRtfState;
var
  AInfo, AFormattingInfo: TdxCharacterFormattingBase;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  AFormattingInfo := Importer.Position.CharacterFormatting;
  AInfo := Importer.DocumentModel.DefaultCharacterProperties.Info;
  AMergedCharacterProperties := TdxMergedCharacterProperties.Create(AInfo.Info, AInfo.Options);
  try
    Importer.ApplyCharacterProperties(Importer.DocumentModel.DefaultCharacterProperties,
      AFormattingInfo.Info, AMergedCharacterProperties, False);
  finally
    AMergedCharacterProperties.Free;
  end;
end;

procedure TdxDefaultCharacterPropertiesDestination.FinalizePieceTableCreation;
begin
end;

function TdxDefaultCharacterPropertiesDestination.CanAppendText: Boolean;
begin
  Result := False;
end;

constructor TdxDefaultCharacterPropertiesDestination.Create(
  AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter, AImporter.PieceTable);
  Importer.Position.CharacterFormatting.DoubleFontSize := 20;
end;

class function TdxDefaultCharacterPropertiesDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class function TdxDefaultCharacterPropertiesDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxDefaultCharacterPropertiesDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxDefaultCharacterPropertiesDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxDefaultCharacterPropertiesDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  AddCharacterPropertiesKeywords(Result);
end;

procedure TdxDefaultCharacterPropertiesDestination.ProcessCharCore(AChar: Char);
begin
end;

{ TdxDefaultParagraphPropertiesDestination }

procedure TdxDefaultParagraphPropertiesDestination.BeforePopRtfState;
var
  AFormattingInfo: TdxRtfParagraphFormattingInfo;
  AInfo: TdxParagraphFormattingBase;
  AMergedProperties: TdxMergedParagraphProperties;
begin
  AFormattingInfo := Importer.Position.ParagraphFormattingInfo;
  AInfo := Importer.DocumentModel.DefaultParagraphProperties.Info;
  AMergedProperties := TdxMergedParagraphProperties.Create(AInfo.Info, AInfo.Options);
  try
    Importer.ApplyLineSpacing(Importer.Position.ParagraphFormattingInfo);
    Importer.ApplyParagraphProperties(
      Importer.DocumentModel.DefaultParagraphProperties,
      AFormattingInfo, AMergedProperties, False);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxDefaultParagraphPropertiesDestination.FinalizePieceTableCreation;
begin
end;

function TdxDefaultParagraphPropertiesDestination.CanAppendText: Boolean;
begin
  Result := False;
end;

constructor TdxDefaultParagraphPropertiesDestination.Create(
  AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter, AImporter.PieceTable);
end;

class function TdxDefaultParagraphPropertiesDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class constructor TdxDefaultParagraphPropertiesDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxDefaultParagraphPropertiesDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxDefaultParagraphPropertiesDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxDefaultParagraphPropertiesDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  AddParagraphPropertiesKeywords(Result);
end;

procedure TdxDefaultParagraphPropertiesDestination.ProcessCharCore(AChar: Char);
begin
end;

end.
