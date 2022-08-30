//---------------------------------------------------------------------------

#ifndef CustomDrawCardViewDemoMainH
#define CustomDrawCardViewDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxGridCardView.hpp"
#include "cxGridDBCardView.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "CustomDrawCardViewDemoTypes.h"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TCustomDrawCardViewDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TSplitter *Splitter;
  TPanel *pnPersonLines;
  TPanel *pnPersonLinesCaption;
  TcxGrid *cxgPersonLine;
  TcxGridDBTableView *tvPersonLine;
  TcxGridDBColumn *tvPersonLineNAME;
  TcxGridLevel *lvPersonLine;
  TPanel *pnPersons;
  TcxGrid *cxgPersons;
  TcxGridDBCardView *cvPersons;
  TcxGridDBCardViewRow *cvPersonsFIRSTNAME;
  TcxGridDBCardViewRow *cvPersonsSECONDNAME;
  TcxGridDBCardViewRow *cvPersonsGENDER;
  TcxGridDBCardViewRow *cvPersonsBIRTHNAME;
  TcxGridDBCardViewRow *cvPersonsDATEOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvPersonsBIOGRAPHY;
  TcxGridDBCardViewRow *cvPersonsNICKNAME;
  TcxGridLevel *lvPersons;
  TPanel *pnPersonsCaption;
  TMenuItem *miOptions;
  TMenuItem *miFont;
  TMenuItem *miCustomDrawStyles;
  TMenuItem *miBackgroundImage;
  TMenuItem *miTile;
  TMenuItem *miSky;
  TMenuItem *miEgypt;
  TMenuItem *miMyFace;
  TMenuItem *miCar;
  TMenuItem *miLoadImage;
  TMenuItem *miGradient;
  TMenuItem *miGrey;
  TMenuItem *miGreen;
  TMenuItem *miGold;
  TMenuItem *miBlue;
  TMenuItem *miDependOnDataDrawing;
  TMenuItem *miDefaultDrawing;
  TImageList *ilPics;
  TOpenDialog *OpenDialog;
  TFontDialog *FontDialog;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *stBlueDark;
  TcxStyle *stGold;
  TcxStyle *stBlueLight;
  TcxStyle *stBlueBright;
  TcxStyle *stYellowLight;
  TcxStyle *stGreyLight;
  TcxStyle *stBlueSky;
  TcxStyle *stDefault;
  void __fastcall cvPersonsCustomDrawCell(TcxCustomGridTableView *Sender,
	TcxCanvas *ACanvas, TcxGridTableDataCellViewInfo *AViewInfo, bool &ADone);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall miFontClick(TObject *Sender);
  void __fastcall miTileClick(TObject *Sender);
  void __fastcall miSkyClick(TObject *Sender);
  void __fastcall miEgyptClick(TObject *Sender);
  void __fastcall miMyFaceClick(TObject *Sender);
  void __fastcall miCarClick(TObject *Sender);
  void __fastcall miLoadClick(TObject *Sender);
  void __fastcall miDependOnDataDrawingClick(TObject *Sender);
  void __fastcall miDefaultDrawingClick(TObject *Sender);
  void __fastcall miGreyClick(TObject *Sender);
  void __fastcall miGreenClick(TObject *Sender);
  void __fastcall miGoldClick(TObject *Sender);
  void __fastcall miBlueClick(TObject *Sender);
private:  // User declarations
  TCustomDrawingStyle FCustomDrawingStyle;
  CustomDrawCardViewDemoTypesH::TColorScheme FColorScheme;
  TFont* FFont;
  Graphics::TBitmap* FCarBitmap;
  Graphics::TBitmap* FSkyBitmap;
  Graphics::TBitmap* FUserDefinedImage;
  Graphics::TBitmap* FCurrentBitmap;
  Graphics::TBitmap* FEgyptBitmap;
  Graphics::TBitmap* FMyFaceBitmap;
  Graphics::TBitmap* FTileBitmap; 
  String __fastcall TextToDraw(TcxGridTableDataCellViewInfo *AViewInfo);
  void __fastcall GridsStyles();
  void __fastcall AssignCustomDrawProc();
  void __fastcall UncheckMenuItem(TMenuItem* AMenuItem);
  void __fastcall SetBkImage(TMenuItem* AMenuItem, Graphics::TBitmap* ABitMap);
  void __fastcall SetGradientColor(TMenuItem* AMenuItem,
	CustomDrawCardViewDemoTypesH::TColorScheme AColorScheme, TcxStyle *AStyle);
public:   // User declarations
  __fastcall TCustomDrawCardViewDemoMainForm(TComponent* Owner);
private:
  bool __fastcall IsLookAndFeelMenuItem(TMenuItem* AMenuItem);
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawCardViewDemoMainForm *CustomDrawCardViewDemoMainForm;
//---------------------------------------------------------------------------
#endif
