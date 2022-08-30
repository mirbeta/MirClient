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

unit dxRichEdit.Export.Formats;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxGenerics,

  dxRichEdit.NativeApi,
  dxRichEdit.Options.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Export.Core;

type

  { TdxExportFileFormat }

  TdxExportFileFormat = class abstract
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; virtual;
    function GetExporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter; virtual; abstract;
    function GetDocumentExporter: IdxExporter; virtual; abstract;
  end;
  TdxExportFileFormatClass = class of TdxExportFileFormat;

  { TdxExportFileFormats }

  TdxExportFileFormats = class sealed
  strict private
    class var
      FExportFileFormats: TdxObjectList<TdxExportFileFormat>;
    class destructor Finalize;
  protected
    class function GetExportFileFormat(ADocumentFormat: TdxRichEditDocumentFormat): TdxExportFileFormat;
  public
    class function GetExporter(ADocumentFormat: TdxRichEditDocumentFormat;
      ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter;
    class function GetFileExtension(ADocumentFormat: TdxRichEditDocumentFormat): string;
    class function IsFormatSupported(ADocumentFormat: TdxRichEditDocumentFormat): Boolean;

    class procedure RegisterFileFormat(AFileFormatClass: TdxExportFileFormatClass);
    class procedure UnRegisterFileFormat(AFileFormatClass: TdxExportFileFormatClass);

    class procedure RegisterDocumentExportFormats(ADocumentImportManager: TdxDocumentExportManagerService);
  end;

implementation

{ TdxExportFileFormat }

class function TdxExportFileFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Undefined;
  AbstractErrorProc;
end;

{ TdxExportFileFormats }

class destructor TdxExportFileFormats.Finalize;
begin
  FreeAndNil(FExportFileFormats);
end;

class function TdxExportFileFormats.GetExporter(ADocumentFormat: TdxRichEditDocumentFormat;
  ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter;
var
  AExportFileFormat: TdxExportFileFormat;
begin
  AExportFileFormat := GetExportFileFormat(ADocumentFormat);
  if AExportFileFormat = nil then
    Result := nil
  else
    Result := AExportFileFormat.GetExporter(ADocumentModel, AOptions);
end;

class function TdxExportFileFormats.GetExportFileFormat(
  ADocumentFormat: TdxRichEditDocumentFormat): TdxExportFileFormat;
var
  AFileFormat: TdxExportFileFormat;
  I: Integer;
begin
  if FExportFileFormats = nil then
    Exit(nil);
  for I := 0 to FExportFileFormats.Count - 1 do
  begin
    AFileFormat := FExportFileFormats[I];
    if AFileFormat.GetDocumentFormat = ADocumentFormat then
      Exit(AFileFormat);
  end;
  Result := nil;
end;

class function TdxExportFileFormats.GetFileExtension(ADocumentFormat: TdxRichEditDocumentFormat): string;
var
  AExportFileFormat: TdxExportFileFormat;
  AExporter: IdxExporter;
  AExtensions: TStrings;
begin
  AExportFileFormat := GetExportFileFormat(ADocumentFormat);
  if AExportFileFormat = nil then
    Exit('');
  AExporter := AExportFileFormat.GetDocumentExporter;
  AExtensions := AExporter.Filter.Extensions;
  if AExtensions.Count > 0 then
    Result := AExtensions[0]
  else
    Result := '';
end;

class function TdxExportFileFormats.IsFormatSupported(ADocumentFormat: TdxRichEditDocumentFormat): Boolean;
begin
  Result := GetExportFileFormat(ADocumentFormat) <> nil;
end;

class procedure TdxExportFileFormats.RegisterDocumentExportFormats(
  ADocumentImportManager: TdxDocumentExportManagerService);
var
  AFileFormat: TdxExportFileFormat;
  ADocumentModelExporter: IdxExporter;
  I: Integer;
begin
  if Assigned(FExportFileFormats) then
    for I := 0 to FExportFileFormats.Count - 1 do
    begin
      AFileFormat := FExportFileFormats[I];
      ADocumentModelExporter := AFileFormat.GetDocumentExporter;
      ADocumentImportManager.RegisterExporter(ADocumentModelExporter);
    end;
end;

class procedure TdxExportFileFormats.RegisterFileFormat(AFileFormatClass: TdxExportFileFormatClass);
begin
  if FExportFileFormats = nil then
    FExportFileFormats := TdxObjectList<TdxExportFileFormat>.Create;
  if GetExportFileFormat(AFileFormatClass.GetDocumentFormat) <> nil then
    Exit;
  FExportFileFormats.Add(AFileFormatClass.Create);
end;

class procedure TdxExportFileFormats.UnRegisterFileFormat(AFileFormatClass: TdxExportFileFormatClass);
var
  AFileFormat: TdxExportFileFormat;
begin
  AFileFormat := GetExportFileFormat(AFileFormatClass.GetDocumentFormat);
  if AFileFormat = nil then
    Exit;
  FExportFileFormats.Remove(AFileFormat);
  if FExportFileFormats.Count = 0 then
    FreeAndNil(FExportFileFormats);
end;

end.

