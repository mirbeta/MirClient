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

unit dxRichEdit.Export.Html;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Forms,

  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Options,
  dxRichEdit.NativeApi,
  dxRichEdit.ServiceManager,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.Export.Core,
  dxRichEdit.Export.Formats,
  dxRichEdit.Export.Html.Types,
  dxRichEdit.Export.Html.Classes,
  dxRichEdit.Export.Html.ContentExporter;

type
  { TdxHtmlExporterBase }

  TdxHtmlExporterBase = class abstract(TdxDocumentModelExporter)
  strict private
    FFilesPath: string;
    FImageRepository: IdxOfficeImageRepository;
    FRelativeUri: string;
    FScriptContainer: IdxScriptContainer;
    FServiceProvider: IdxServiceProvider;
    FStyleControl: TdxStyleWebControl;
    procedure WriteStyleLink(AControl: TdxWebControlBase);
  protected
    procedure Initialize(const ATargetUri: string; AUseAbsolutePath: Boolean); virtual;

    procedure CreateHtmlDocument(AWriter: TdxHtmlTextWriter; ABody: TdxWebControlBase); virtual;
    function CreateTitle: TdxWebControlBase; virtual;
    function GetScriptContainer: IdxScriptContainer; virtual;
    procedure ExportCore(AWriter: TTextWriter); virtual;
    function ExportBodyControl: TdxWebControlBase; virtual;
    procedure ExportBodyContent(ARoot: TdxWebControlBase); virtual; abstract;
    function ExportCssProperiesToSeparateFile(AStyle: TdxStyleWebControl): string;
    procedure SetupBodyTag(ABody: TdxWebControlBase); virtual;
    function ShouldWriteStyles: Boolean;
    procedure WriteHtmlDocumentPreamble(AWriter: TdxHtmlTextWriter); virtual;
    procedure WriteStyles(AControl: TdxWebControlBase);

    function EmbedImages: Boolean; virtual; abstract;
    function ExportToBodyTag: Boolean; virtual; abstract;
    function ExportStylesAsStyleTag: Boolean; virtual; abstract;
    function ExportStylesAsLink: Boolean; virtual; abstract;
    function Encoding: TEncoding; virtual; abstract;
    function UseHtml5: Boolean; virtual;

    property FilesPath: string read FFilesPath;
    property ImageRepository: IdxOfficeImageRepository read FImageRepository;
    property RelativeUri: string read FRelativeUri;
    property ScriptContainer: IdxScriptContainer read FScriptContainer;
    property ServiceProvider: IdxServiceProvider read FServiceProvider;
    property StyleControl: TdxStyleWebControl read FStyleControl;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions); override;
    destructor Destroy; override;
  end;

  { TdxHtmlExporter }

  TdxHtmlExporter = class(TdxHtmlExporterBase)
  strict private
    FContentExporter: TdxHtmlContentExporter;
    FOptions: TdxHtmlDocumentExporterOptions;
  protected
    procedure Export(AWriter: TTextWriter); overload;
    function EmbedImages: Boolean; override;
    function ExportToBodyTag: Boolean; override;
    function ExportStylesAsStyleTag: Boolean; override;
    function ExportStylesAsLink: Boolean; override;
    function Encoding: TEncoding; override;

    function CreateContentExporter(ADocumentModel: TdxDocumentModel;
      AOptions: TdxHtmlDocumentExporterOptions): TdxHtmlContentExporter;
    function CreateTitle: TdxWebControlBase; override;
    procedure ExportBodyContent(ARoot: TdxWebControlBase); override;
    procedure SetupBodyTag(ABody: TdxWebControlBase); override;
    function UseHtml5: Boolean; override;

    property ContentExporter: TdxHtmlContentExporter read FContentExporter;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; AOptions: TdxHtmlDocumentExporterOptions); reintroduce;
    destructor Destroy; override;
    procedure Export(AOutputStream: TStream); overload; override;
    function ExportSaveMemory: TdxChunkedStringBuilder; override;

    property Options: TdxHtmlDocumentExporterOptions read FOptions;
  end;

  { TdxExportHtmlFormat }

  TdxExportHtmlFormat = class(TdxExportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetExporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter; override;
    function GetDocumentExporter: IdxExporter; override;
  end;

implementation

uses
  IOUtils,
  dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxEncoding,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.UriStreamService,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Export.Html.Utils,
  dxRichEdit.Export.Html.DocumentExporter;

type
  { TdxServiceBasedImageRepository }

  TdxServiceBasedImageRepository = class(TInterfacedObject, IdxOfficeImageRepository)
  strict private
    FServiceProvider: IdxServiceProvider;
    FRootUri: string;
    FRelativeUri: string;
  public
    constructor Create(const AServiceProvider: IdxServiceProvider;
      const ARootUri, ARelativeUri: string);
    function GetImageSource(AImage: TdxOfficeImageReference): string;
  end;


{ TdxServiceBasedImageRepository }

constructor TdxServiceBasedImageRepository.Create(
  const AServiceProvider: IdxServiceProvider; const ARootUri, ARelativeUri: string);
begin
  inherited Create;
  FServiceProvider := AServiceProvider;
  FRootUri := ARootUri;
  FRelativeUri := ARelativeUri;
end;

function TdxServiceBasedImageRepository.GetImageSource(AImage: TdxOfficeImageReference): string;
var
  AService: IdxUriProviderService;
begin
  Result := '';
  AService := FServiceProvider.GetService(IdxUriProviderService) as IdxUriProviderService;
  if AService <> nil then
    Result := AService.CreateImageUri(FRootUri, AImage.Image, FRelativeUri);
end;

{ TdxHtmlExporterBase }

constructor TdxHtmlExporterBase.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FServiceProvider := ADocumentModel;
  FStyleControl := TdxStyleWebControl.Create;
end;

destructor TdxHtmlExporterBase.Destroy;
begin
  FScriptContainer := nil;
  FreeAndNil(FStyleControl);
  inherited Destroy;
end;

procedure TdxHtmlExporterBase.CreateHtmlDocument(AWriter: TdxHtmlTextWriter; ABody: TdxWebControlBase);
var
  AHtml, AHead, AMeta: TdxHtmlGenericControl;
  ATitle: TdxWebControlBase;
begin
  AHtml := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Html);
  try
    if not UseHtml5 then
      AHtml.Attributes.Add('xmlns', 'http://www.w3.org/1999/xhtml');
    AHead := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Head);
    try
      AHtml.Controls.Add(AHead);
      AMeta := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Meta);
      try
        AMeta.Attributes.Add('http-equiv', 'Content-Type');
        AMeta.Attributes.Add('content', Format('text/html; charset=%s', [Encoding.WebName]));
        AHead.Controls.Add(AMeta);
        ATitle := CreateTitle;
        try
          AHead.Controls.Add(ATitle);

          if ShouldWriteStyles then
            WriteStyles(AHead);

          AHtml.Controls.Add(ABody);

          WriteHtmlDocumentPreamble(AWriter);
          AWriter.WriteLine;
          AHtml.RenderControl(AWriter);
        finally
          ATitle.Free;
        end;
      finally
        AMeta.Free;
      end;
    finally
      AHead.Free;
    end;
  finally
    AHtml.Free;
  end;
end;

function TdxHtmlExporterBase.CreateTitle: TdxWebControlBase;
begin
  Result := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Title);
end;

function TdxHtmlExporterBase.GetScriptContainer: IdxScriptContainer;
begin
  Result := ServiceProvider.GetService(IdxScriptContainer) as IdxScriptContainer;
  if Result <> nil then
    Exit;
  Result := StyleControl;
end;

procedure TdxHtmlExporterBase.ExportCore(AWriter: TTextWriter);
var
  AHtmlWriter: TdxHtmlTextWriter;
  ABody: TdxWebControlBase;
begin
  AHtmlWriter := TdxHtmlTextWriter.Create(AWriter);
  try
    ABody := ExportBodyControl;
    try
      if ExportToBodyTag then
        ABody.RenderControl(AHtmlWriter)
      else
        CreateHtmlDocument(AHtmlWriter, ABody);
    finally
      ABody.Free;
    end;
  finally
    AHtmlWriter.Free;
  end;
end;

function TdxHtmlExporterBase.ExportBodyControl: TdxWebControlBase;
var
  ARoot: TdxEmptyWebControl;
begin
  ARoot := TdxEmptyWebControl.Create;
  ExportBodyContent(ARoot);
  Result := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Body, ARoot);
  SetupBodyTag(Result);
  if ExportToBodyTag and ShouldWriteStyles then
    WriteStyles(Result);
  Result.Controls.Add(ARoot);
end;

function TdxHtmlExporterBase.ExportCssProperiesToSeparateFile(AStyle: TdxStyleWebControl): string;
var
  ASb: TStringBuilder;
  AWriter: TStringWriter;
  AStyleText: string;
  AService: IdxUriProviderService;
begin
  ASb := TStringBuilder.Create;
  try
    AWriter := TStringWriter.Create(ASb);
    try
      AStyle.RenderStyles(AWriter);
      AWriter.Flush;
    finally
      AWriter.Free;
    end;
    AStyleText := ASb.ToString;
    AService := ServiceProvider.GetService(IdxUriProviderService) as IdxUriProviderService;
    if AService <> nil then
      Exit(AService.CreateCssUri(FilesPath, AStyleText, RelativeUri));
  finally
    ASb.Free;
  end;
  Result := '';
end;

procedure TdxHtmlExporterBase.SetupBodyTag(ABody: TdxWebControlBase);
begin
end;

procedure TdxHtmlExporterBase.Initialize(const ATargetUri: string; AUseAbsolutePath: Boolean);
var
  S: string;
begin
  if ATargetUri = '' then
    FFilesPath := '_files/'
  else
  begin
    S := TPath.GetDirectoryName(ATargetUri);
    if S = '' then
      S := TPath.GetDirectoryName(Application.ExeName);
    FFilesPath := S + '/' + TPath.GetFileNameWithoutExtension(ATargetUri) + '_files/';
  end;
  if AUseAbsolutePath then
    FRelativeUri := ''
  else
    FRelativeUri := TPath.GetFileNameWithoutExtension(ATargetUri) + '_files/';

  FScriptContainer := GetScriptContainer;
  FImageRepository := TdxServiceBasedImageRepository.Create(ServiceProvider, FilesPath, RelativeUri);
end;

function TdxHtmlExporterBase.ShouldWriteStyles: Boolean;
begin
  Result := (StyleControl.Styles.Count > 0) or (TObject(ScriptContainer) <> StyleControl);
end;

function TdxHtmlExporterBase.UseHtml5: Boolean;
begin
  Result := False;
end;

procedure TdxHtmlExporterBase.WriteHtmlDocumentPreamble(
  AWriter: TdxHtmlTextWriter);
begin
  if UseHtml5 then
    AWriter.Write('<!DOCTYPE html>')
  else
    AWriter.Write('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
end;

procedure TdxHtmlExporterBase.WriteStyleLink(AControl: TdxWebControlBase);
var
  AResult: string;
  ALink: TdxHtmlGenericControl;
begin
  AResult := ExportCssProperiesToSeparateFile(StyleControl);
  ALink := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Link);
  ALink.Attributes.Add('rel', 'stylesheet');
  ALink.Attributes.Add('type', 'text/css');
  ALink.Attributes.Add('href', AResult);
  AControl.Controls.Add(ALink);
end;

procedure TdxHtmlExporterBase.WriteStyles(AControl: TdxWebControlBase);
begin
  if ExportStylesAsStyleTag then
    AControl.Controls.Add(StyleControl);
  if ExportStylesAsLink then
    WriteStyleLink(AControl);
end;

{ TdxHtmlExporter }

constructor TdxHtmlExporter.Create(ADocumentModel: TdxDocumentModel;
  AOptions: TdxHtmlDocumentExporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FOptions := AOptions;
  Initialize(FOptions.TargetUri, FOptions.UriExportType = TdxUriExportType.Absolute);
  FContentExporter := CreateContentExporter(ADocumentModel, FOptions);
end;

destructor TdxHtmlExporter.Destroy;
begin
  FreeAndNil(FContentExporter);
  inherited Destroy;
end;

procedure TdxHtmlExporter.Export(AOutputStream: TStream);
var
  AUriProvider: IdxUriProvider;
  AService: IdxUriProviderService;
  AWriter: TStreamWriter;
  AStreamPosition: Int64;
begin
  AStreamPosition := AOutputStream.Position;
  AWriter := TStreamWriter.Create(AOutputStream, Options.ActualEncoding);
  try
    AWriter.BaseStream.Position := AStreamPosition;
    if EmbedImages then
    begin
      AUriProvider := TdxDataStringUriProvider.Create;
      AService := ServiceProvider.GetService(IdxUriProviderService) as IdxUriProviderService;
      if AService <> nil then
        AService.RegisterProvider(AUriProvider);
    end;
    try
      ExportCore(AWriter);
    finally
      AWriter.Flush;
      if AService <> nil then
        AService.UnregisterProvider(AUriProvider);
    end;
  finally
    AWriter.Free;
  end;
end;

function TdxHtmlExporter.EmbedImages: Boolean;
begin
  Result := Options.EmbedImages;
end;

function TdxHtmlExporter.ExportToBodyTag: Boolean;
begin
  Result := Options.ExportRootTag = TdxExportRootTag.Body;
end;

function TdxHtmlExporter.ExportStylesAsStyleTag: Boolean;
begin
  Result := Options.CssPropertiesExportType = TdxCssPropertiesExportType.Style;
end;

function TdxHtmlExporter.ExportSaveMemory: TdxChunkedStringBuilder;
var
  AWriter: TdxChunkedStringBuilderWriter;
begin
  Result := TdxChunkedStringBuilder.Create;
  AWriter := TdxChunkedStringBuilderWriter.Create(Result);
  try
    Export(AWriter);
  finally
    AWriter.Free;
  end;
end;

function TdxHtmlExporter.ExportStylesAsLink: Boolean;
begin
  Result := Options.CssPropertiesExportType = TdxCssPropertiesExportType.Link;
end;

function TdxHtmlExporter.Encoding: TEncoding;
begin
  Result := Options.ActualEncoding;
end;

function TdxHtmlExporter.CreateContentExporter(ADocumentModel: TdxDocumentModel; AOptions: TdxHtmlDocumentExporterOptions): TdxHtmlContentExporter;
begin
  Result := TdxHtmlContentExporter.Create(ADocumentModel, ScriptContainer, ImageRepository, AOptions);
end;

function TdxHtmlExporter.CreateTitle: TdxWebControlBase;
var
  ATitle: TdxHtmlGenericControl;
begin
  ATitle := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Title);
  Result := ATitle;
end;


procedure TdxHtmlExporter.Export(AWriter: TTextWriter);
var
  AUriProvider: IdxUriProvider;
  AService: IdxUriProviderService;
begin
  if EmbedImages then
  begin
    AUriProvider := TdxDataStringUriProvider.Create;
    AService := ServiceProvider.GetService(IdxUriProviderService) as IdxUriProviderService;
    if AService <> nil then
      AService.RegisterProvider(AUriProvider);
    try
      ExportCore(AWriter);
    finally
      AWriter.Flush;
      if AService <> nil then
        AService.UnregisterProvider(AUriProvider);
    end;
  end
  else
    ExportCore(AWriter);

  AWriter.Flush;
end;

procedure TdxHtmlExporter.ExportBodyContent(ARoot: TdxWebControlBase);
begin
  FContentExporter.Export(ARoot);
end;

procedure TdxHtmlExporter.SetupBodyTag(ABody: TdxWebControlBase);
var
  ABodyControl: TdxHtmlGenericControl;
  APageBackColor: TdxAlphaColor;
  AWebSettings: TdxWebSettings;
  AStyle: TdxCssStyleCollection;
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  ABodyControl := TdxHtmlGenericControl(ABody);
  APageBackColor := DocumentModel.DocumentProperties.PageBackColor;
  if not TdxAlphaColors.IsEmpty(APageBackColor) then
    ABodyControl.Attributes.Add('bgcolor', TdxHtmlConvert.ToHtml(APageBackColor));
  AWebSettings := DocumentModel.WebSettings;
  if AWebSettings.IsBodyMarginsSet then
  begin
    AStyle := ABodyControl.Style;
    AUnitConverter := DocumentModel.UnitConverter;
    if AWebSettings.LeftMargin <> 0 then
      AStyle.Add(TdxHtmlTextWriterStyle.MarginLeft, IntToStr(AUnitConverter.ModelUnitsToPixels(AWebSettings.LeftMargin, DocumentModel.DPI)) + 'px');
    if AWebSettings.TopMargin <> 0 then
      AStyle.Add(TdxHtmlTextWriterStyle.MarginTop, IntToStr(AUnitConverter.ModelUnitsToPixels(AWebSettings.TopMargin, DocumentModel.DPI)) + 'px');
    if AWebSettings.RightMargin <> 0 then
      AStyle.Add(TdxHtmlTextWriterStyle.MarginRight, IntToStr(AUnitConverter.ModelUnitsToPixels(AWebSettings.RightMargin, DocumentModel.DPI)) + 'px');
    if AWebSettings.BottomMargin <> 0 then
      AStyle.Add(TdxHtmlTextWriterStyle.MarginBottom, IntToStr(AUnitConverter.ModelUnitsToPixels(AWebSettings.BottomMargin, DocumentModel.DPI)) + 'px');
  end;
end;

function TdxHtmlExporter.UseHtml5: Boolean;
begin
  Result := FOptions.UseHtml5;
end;

{ TdxExportHtmlFormat }

function TdxExportHtmlFormat.GetDocumentExporter: IdxExporter;
begin
  Result := TdxHtmlDocumentExporter.Create;
end;

class function TdxExportHtmlFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Html;
end;

function TdxExportHtmlFormat.GetExporter(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter;
begin
  Result := TdxHtmlExporter.Create(TdxDocumentModel(ADocumentModel), AOptions as TdxHtmlDocumentExporterOptions);
end;

end.
