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

unit dxSpreadSheetPackedFileFormatCore;

{$I cxVer.Inc}

interface

uses
  Windows, SysUtils, Classes, dxSpreadSheetCore, dxSpreadSheetClasses, dxZIPUtils, dxXMLDoc, dxCustomTree,
  dxSpreadSheetTypes, dxGDIPlusClasses;

type
  TdxSpreadSheetCustomPackedReader = class;
  TdxSpreadSheetCustomPackedWriter = class;

  { TdxSpreadSheetXMLNode }

  TdxSpreadSheetXMLNode = class(TdxXMLPackableNode)
  protected
    procedure CheckTextEncoding; override;
    function GetNodeClass: TdxXMLNodeClass; override;
  end;

  { TdxSpreadSheetXMLDocument }

  TdxSpreadSheetXMLDocument = class(TdxXMLPackableDocument)
  protected
    function CreateRootNode: TdxXMLNode; override;
  public
    constructor Create(AOwner: TPersistent); override;
  end;

  { TdxSpreadSheetCustomPackedReader }

  TdxSpreadSheetCustomPackedReader = class(TdxSpreadSheetCustomReader)
  strict private
    FPackageReader: TdxZIPStreamReader;

    function CheckFileName(const AFileName: AnsiString): AnsiString;
  protected
    function CreateXML: TdxSpreadSheetXMLDocument; virtual;
    function FileExists(const AFileName: AnsiString): Boolean;
    function ReadFile(const AFileName: AnsiString): TMemoryStream; virtual;
    function ReadXML(const AFileName: AnsiString): TdxXMLDocument; virtual;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    //
    property PackageReader: TdxZIPStreamReader read FPackageReader;
  end;

  { TdxSpreadSheetCustomPackedReaderParser }

  TdxSpreadSheetCustomPackedReaderParser = class(TdxSpreadSheetCustomFilerSubTask)
  protected
    function ReadXML(const AFileName: AnsiString): TdxXMLDocument; inline;
  end;

  { TdxSpreadSheetCustomPackedWriter }

  TdxSpreadSheetCustomPackedWriter = class(TdxSpreadSheetCustomWriter)
  strict private
    FPackageWriter: TdxZIPStreamWriter;
  protected
    procedure WriteFile(const AFileName: AnsiString; AStream: TStream; AFreeStream: Boolean); virtual;
    procedure WriteXML(const AFileName: AnsiString; ADocument: TdxXMLDocument); virtual;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    //
    property PackageWriter: TdxZIPStreamWriter read FPackageWriter;
  end;

  { TdxSpreadSheetCustomPackedWriterBuilder }

  TdxSpreadSheetCustomPackedWriterBuilder = class(TdxSpreadSheetCustomFilerSubTask)
  protected
    procedure WriteFile(const AFileName: AnsiString; AStream: TStream; AFreeStream: Boolean = False); inline;
    procedure WriteXML(const AFileName: AnsiString; ADocument: TdxXMLDocument); inline;
  end;

implementation

uses
  dxCore, dxSpreadSheetStrs, Math, dxSpreadSheetCoreStrs;

{ TdxSpreadSheetXMLNode }

procedure TdxSpreadSheetXMLNode.CheckTextEncoding;
begin
  if Length(Text) > MAXSHORT then
  begin
    Text := Copy(Text, 1, MAXSHORT);
    Text := dxStringToXMLString(dxXMLStringToString(Text));
  end;
  inherited CheckTextEncoding;
end;

function TdxSpreadSheetXMLNode.GetNodeClass: TdxXMLNodeClass;
begin
  Result := TdxSpreadSheetXMLNode;
end;

{ TdxSpreadSheetXMLDocument }

constructor TdxSpreadSheetXMLDocument.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  Standalone := 'yes';
end;

function TdxSpreadSheetXMLDocument.CreateRootNode: TdxXMLNode;
begin
  Result := TdxSpreadSheetXMLNode.Create;
end;

{ TdxSpreadSheetCustomPackedReader }

constructor TdxSpreadSheetCustomPackedReader.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FPackageReader := TdxZIPStreamReader.Create(Stream);
end;

destructor TdxSpreadSheetCustomPackedReader.Destroy;
begin
  FreeAndNil(FPackageReader);
  inherited Destroy;
end;

function TdxSpreadSheetCustomPackedReader.CreateXML: TdxSpreadSheetXMLDocument;
begin
  Result := TdxSpreadSheetXMLDocument.Create(nil);
end;

function TdxSpreadSheetCustomPackedReader.FileExists(const AFileName: AnsiString): Boolean;
begin
  Result := PackageReader.Exists(CheckFileName(AFileName));
end;

function TdxSpreadSheetCustomPackedReader.ReadFile(const AFileName: AnsiString): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    if PackageReader.Extract(CheckFileName(AFileName), Result) then
      Result.Position := 0
    else
      DoError(sdxErrorFileCannotBeFoundInPackage, [AFileName], ssmtError);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TdxSpreadSheetCustomPackedReader.ReadXML(const AFileName: AnsiString): TdxXMLDocument;
var
  AStream: TMemoryStream;
begin
  AStream := ReadFile(AFileName);
  try
    Result := CreateXML;
    Result.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

function TdxSpreadSheetCustomPackedReader.CheckFileName(const AFileName: AnsiString): AnsiString;
begin
  if (AFileName <> '') and (AFileName[1] = '.') then
    Result := Copy(AFileName, 2, MaxInt)
  else
    Result := AFileName;
end;

{ TdxSpreadSheetCustomPackedReaderParser }

function TdxSpreadSheetCustomPackedReaderParser.ReadXML(const AFileName: AnsiString): TdxXMLDocument;
begin
  Result := TdxSpreadSheetCustomPackedReader(Owner).ReadXML(AFileName);
end;

{ TdxSpreadSheetCustomPackedWriter }

constructor TdxSpreadSheetCustomPackedWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FPackageWriter := TdxZIPStreamWriter.Create(Stream);
end;

destructor TdxSpreadSheetCustomPackedWriter.Destroy;
begin
  FreeAndNil(FPackageWriter);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomPackedWriter.WriteFile(const AFileName: AnsiString; AStream: TStream; AFreeStream: Boolean);
begin
  PackageWriter.AddFile(AFileName, AStream, AFreeStream);
end;

procedure TdxSpreadSheetCustomPackedWriter.WriteXML(const AFileName: AnsiString; ADocument: TdxXMLDocument);
var
  AStream: TdxCompressedStream;
begin
  AStream := TdxCompressedStream.Create;
  ADocument.SaveToStream(AStream);
  WriteFile(AFileName, AStream, True);
end;

{ TdxSpreadSheetCustomPackedWriterBuilder }

procedure TdxSpreadSheetCustomPackedWriterBuilder.WriteFile(
  const AFileName: AnsiString; AStream: TStream; AFreeStream: Boolean = False);
begin
  TdxSpreadSheetCustomPackedWriter(Owner).WriteFile(AFileName, AStream, AFreeStream);
end;

procedure TdxSpreadSheetCustomPackedWriterBuilder.WriteXML(const AFileName: AnsiString; ADocument: TdxXMLDocument);
begin
  TdxSpreadSheetCustomPackedWriter(Owner).WriteXML(AFileName, ADocument);
end;

end.
