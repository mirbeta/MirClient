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

unit dxRichEdit.Import.OpenXML.DestinationSettings;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  SysUtils, Classes,
  dxProtectionUtils,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxXMLReader,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML;

type

  { TdxDocumentSettingsDestination }

  TdxDocumentSettingsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnAutoHyphenation(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDefaultTabStop(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDifferentOddAndEvenPages(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDocumentProtection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDocumentVariables(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFootNoteProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnEndNoteProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDisplayBackgroundShape(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxDocumentSettingsAutoHyphenationDestination }

  TdxDocumentSettingsAutoHyphenationDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDocumentSettingsDefaultTabStopDestination }

  TdxDocumentSettingsDefaultTabStopDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDocumentSettingsDifferentOddAndEvenPagesDestination }

  TdxDocumentSettingsDifferentOddAndEvenPagesDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDocumentProtectionDestination }

  TdxDocumentProtectionDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDocumentVariablesDestination }

  TdxDocumentVariablesDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnDocumentVariable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxDocumentVariableDestination }

  TdxDocumentVariableDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDisplayBackgroundShapeDestination }

  TdxDisplayBackgroundShapeDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Contnrs, Math, dxCore, dxGenerics,

  dxBase64,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.Import.OpenXML.DestinationFootNote,
  dxRichEdit.Import.OpenXML.DestinationEndNote;

{ TdxDocumentSettingsDestination }

class constructor TdxDocumentSettingsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDocumentSettingsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDocumentSettingsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('autoHyphenation', OnAutoHyphenation);
  Result.Add('defaultTabStop', OnDefaultTabStop);
  Result.Add('evenAndOddHeaders', OnDifferentOddAndEvenPages);
  Result.Add('documentProtection', OnDocumentProtection);
  Result.Add('docVars', OnDocumentVariables);
  Result.Add('footnotePr', OnFootNoteProperties);
  Result.Add('endnotePr', OnEndNoteProperties);
  Result.Add('displayBackgroundShape', OnDisplayBackgroundShape);
end;

function TdxDocumentSettingsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDocumentSettingsDestination.OnAutoHyphenation(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentSettingsAutoHyphenationDestination.Create(AImporter);
end;

class function TdxDocumentSettingsDestination.OnDefaultTabStop(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentSettingsDefaultTabStopDestination.Create(AImporter);
end;

class function TdxDocumentSettingsDestination.OnDifferentOddAndEvenPages(
 AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentSettingsDifferentOddAndEvenPagesDestination.Create(AImporter);
end;

class function TdxDocumentSettingsDestination.OnDocumentProtection(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentProtectionDestination.Create(AImporter);
end;

class function TdxDocumentSettingsDestination.OnDocumentVariables(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentVariablesDestination.Create(AImporter);
end;

class function TdxDocumentSettingsDestination.OnFootNoteProperties(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentLevelFootNotePropertiesDestination.Create(AImporter, AImporter.DocumentModel.Sections.First.FootNote);
end;

class function TdxDocumentSettingsDestination.OnEndNoteProperties(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentLevelEndNotePropertiesDestination.Create(AImporter, AImporter.DocumentModel.Sections.First.EndNote);
end;

class function TdxDocumentSettingsDestination.OnDisplayBackgroundShape(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDisplayBackgroundShapeDestination.Create(AImporter);
end;

{ TdxDocumentSettingsAutoHyphenationDestination }

procedure TdxDocumentSettingsAutoHyphenationDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.DocumentModel.DocumentProperties.HyphenateDocument := Importer.GetWpSTOnOffValue(AReader, 'val', False);
end;

{ TdxDocumentSettingsDefaultTabStopDestination }

procedure TdxDocumentSettingsDefaultTabStopDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val', MinInt);
  if AValue > 0 then
    Importer.DocumentModel.DocumentProperties.DefaultTabWidth := Max(1, UnitConverter.TwipsToModelUnits(AValue));
end;

{ TdxDocumentSettingsDifferentOddAndEvenPagesDestination }

procedure TdxDocumentSettingsDifferentOddAndEvenPagesDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.DocumentModel.DocumentProperties.DifferentOddAndEvenPages := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxDocumentProtectionDestination }

procedure TdxDocumentProtectionDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AProperties: TdxDocumentProtectionProperties;
  ATextValue : string;
  AHash: Integer;
  AValue: Cardinal;
  APasswordHash: TArray<Byte>;
begin
  AProperties := Importer.DocumentModel.ProtectionProperties;
  AProperties.BeginInit;
  try
    ATextValue := Importer.ReadAttribute(AReader, 'hash');
    if ATextValue <> '' then
      AProperties.PasswordHash := TdxBase64.FromBase64String(ATextValue);

    ATextValue := Importer.ReadAttribute(AReader, 'salt');
    if ATextValue <> '' then
      AProperties.PasswordPrefix := TdxBase64.FromBase64String(ATextValue);

    ATextValue := Importer.ReadAttribute(AReader, 'unprotectPassword');
    if ATextValue <> '' then
    begin
      if ATextValue <> '00000000' then
      begin
        if TdxNumber.TryParse(ATextValue, TdxNumberStyles.HexNumber, AHash) then
        begin
          AValue := Cardinal(AHash);
          AHash := Integer(
            ((AValue shl 24) and $FF000000) or
            ((AValue shl 8) and $00FF0000) or
            ((AValue shr 8) and $0000FF00) or
            ((AValue shr 24) and $000000FF));
          SetLength(APasswordHash, SizeOf(AHash));
          Move(AHash, APasswordHash[0], SizeOf(AHash));
          AProperties.Word2003PasswordHash := APasswordHash;
        end;
      end;
    end;

    ATextValue := Importer.ReadAttribute(AReader, 'edit');
    if (ATextValue = 'readOnly') or (ATextValue = 'read-only') then
      AProperties.ProtectionType := TdxDocumentProtectionType.ReadOnly;
    AProperties.EnforceProtection := Importer.GetWpSTOnOffValue(AReader, 'enforcement', False);
    AProperties.HashAlgorithmType := TdxHashAlgorithmType(Importer.GetWpSTIntegerValue(AReader, 'cryptAlgorithmSid', 0));
    AProperties.HashIterationCount := Importer.GetWpSTIntegerValue(AReader, 'cryptSpinCount', 1);
  finally
    AProperties.EndInit;
  end;
end;

{ TdxDocumentVariablesDestination }

class constructor TdxDocumentVariablesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDocumentVariablesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDocumentVariablesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('docVar', OnDocumentVariable);
end;

function TdxDocumentVariablesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDocumentVariablesDestination.OnDocumentVariable(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentVariableDestination.Create(AImporter);
end;

{ TdxDocumentVariableDestination }

procedure TdxDocumentVariableDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AName, AVal: string;
begin
  AName := Importer.ReadAttribute(AReader, 'name');
  AVal := Importer.ReadAttribute(AReader, 'val');
  if (AName <> '') and (AVal <> '') then
  begin
    AVal := Importer.DecodeXmlChars(AVal);
    Importer.DocumentModel.Variables.Add(AName, AVal);
  end;
end;

{ TdxDisplayBackgroundShapeDestination }

procedure TdxDisplayBackgroundShapeDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.DocumentModel.DocumentProperties.DisplayBackgroundShape := Importer.GetWpSTOnOffValue(AReader, 'val', True);
end;

end.
