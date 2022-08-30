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

unit dxRichEdit.Import.Formats;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,

  dxRichEdit.NativeApi,
  dxGenerics,
  dxRichEdit.Options.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Import.Core;

type
  { TdxImportFileFormat }

  TdxImportFileFormatClass = class of TdxImportFileFormat;
  TdxImportFileFormat = class abstract
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; virtual;
    function GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter; virtual; abstract;
    function GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>; virtual; abstract;
  end;

  { TdxImportFileFormats }

  TdxImportFileFormats = class sealed
  strict private
    class var
      FImportFileFormats: TdxObjectList<TdxImportFileFormat>;
    class destructor Finalize;
  protected
    class function GetImportFileFormat(ADocumentFormat: TdxRichEditDocumentFormat): TdxImportFileFormat;
  public
    class function GetImporter(ADocumentFormat: TdxRichEditDocumentFormat; ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter;
    class function IsFormatSupported(ADocumentFormat: TdxRichEditDocumentFormat): Boolean;

    class procedure RegisterFileFormat(AFileFormatClass: TdxImportFileFormatClass);
    class procedure UnRegisterFileFormat(AFileFormatClass: TdxImportFileFormatClass);

    class procedure RegisterDocumentImportFormats(ADocumentImportManager: TdxDocumentImportManagerService);
  end;


implementation

{ TdxImportFileFormat }

class function TdxImportFileFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Undefined;
  AbstractErrorProc;
end;

{ TdxImportFileFormats }

class function TdxImportFileFormats.GetImporter(ADocumentFormat: TdxRichEditDocumentFormat; ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxImporterOptions): TdxDocumentModelImporter;
var
  AImportExportFileFormat: TdxImportFileFormat;
begin
  AImportExportFileFormat := GetImportFileFormat(ADocumentFormat);
  if AImportExportFileFormat = nil then
    Result := nil
  else
    Result := AImportExportFileFormat.GetImporter(ADocumentModel, AOptions);
end;

class function TdxImportFileFormats.GetImportFileFormat(
  ADocumentFormat: TdxRichEditDocumentFormat): TdxImportFileFormat;
var
  AFileFormat: TdxImportFileFormat;
  I: Integer;
begin
  if FImportFileFormats = nil then
    Exit(nil);
  for I := 0 to FImportFileFormats.Count - 1 do
  begin
    AFileFormat := FImportFileFormats[I];
    if AFileFormat.GetDocumentFormat = ADocumentFormat then
      Exit(AFileFormat);
  end;
  Result := nil;
end;

class function TdxImportFileFormats.IsFormatSupported(ADocumentFormat: TdxRichEditDocumentFormat): Boolean;
begin
  Result := GetImportFileFormat(ADocumentFormat) <> nil;
end;

class procedure TdxImportFileFormats.RegisterFileFormat(AFileFormatClass: TdxImportFileFormatClass);
begin
  if FImportFileFormats = nil then
    FImportFileFormats := TdxObjectList<TdxImportFileFormat>.Create;
  if GetImportFileFormat(AFileFormatClass.GetDocumentFormat) <> nil then
    Exit;
  FImportFileFormats.Add(AFileFormatClass.Create);
end;

class procedure TdxImportFileFormats.RegisterDocumentImportFormats(ADocumentImportManager: TdxDocumentImportManagerService);
var
  AFileFormat: TdxImportFileFormat;
  ADocumentModelImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>;
  I: Integer;
begin
  if Assigned(FImportFileFormats) then
    for I := 0 to FImportFileFormats.Count - 1 do
    begin
      AFileFormat := FImportFileFormats[I];
      ADocumentModelImporter := AFileFormat.GetDocumentImporter;
      ADocumentImportManager.RegisterImporter(ADocumentModelImporter);
    end;
end;

class procedure TdxImportFileFormats.UnRegisterFileFormat(AFileFormatClass: TdxImportFileFormatClass);
var
  AFileFormat: TdxImportFileFormat;
begin
  AFileFormat := GetImportFileFormat(AFileFormatClass.GetDocumentFormat);
  if AFileFormat = nil then
    Exit;
  FImportFileFormats.Remove(AFileFormat);
  if FImportFileFormats.Count = 0 then
    FreeAndNil(FImportFileFormats);
end;

class destructor TdxImportFileFormats.Finalize;
begin
  FreeAndNil(FImportFileFormats);
end;

end.
