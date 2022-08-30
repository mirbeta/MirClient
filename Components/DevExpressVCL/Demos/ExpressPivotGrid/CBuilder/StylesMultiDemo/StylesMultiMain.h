//---------------------------------------------------------------------------

#ifndef StylesMultiMainH
#define StylesMultiMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxPivotGridStyleSheetsPreview.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxListBox.hpp"
#include "cxRadioGroup.hpp"
#include "cxStyles.hpp"
#include <ExtCtrls.hpp>
#include "cxCustomData.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TfrmStylesMulti : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
        TcxDBPivotGrid *DBPivotGrid;
        TcxDBPivotGridField *PivotGridPurchaseDate;
        TcxDBPivotGridField *PivotGridPaymentType;
        TcxDBPivotGridField *PivotGridQuantity;
        TcxDBPivotGridField *PivotGridCarName;
        TcxDBPivotGridField *PivotGridUnitPrice;
        TcxDBPivotGridField *PivotGridCompanyName;
        TcxDBPivotGridField *PivotGridPaymentAmount;
        TPanel *pnlLeft;
        TcxGroupBox *gbUserDefined;
        TComboBox *cbUserStyleSheets;
        TcxButton *btnLoad;
        TcxButton *btnSave;
        TcxButton *btnEdit;
        TcxRadioGroup *RadioGroup;
        TcxGroupBox *gbPredefined;
        TcxListBox *lbPredefinedStyleSheets;
        TPanel *pnlCurrentStyleSheet;
        TOpenDialog *OpenDialog;
        TSaveDialog *SaveDialog1;
        TcxStyleRepository *srPredefined;
        TcxStyle *ClassicBackground;
        TcxStyle *ClassicContent;
        TcxStyle *ClassicHeader;
        TcxStyle *ClassicInactive;
        TcxStyle *ClassicSelection;
        TcxStyle *cxStyle2;
        TcxStyle *cxStyle3;
        TcxStyle *cxStyle6;
        TcxStyle *cxStyle10;
        TcxStyle *cxStyle13;
        TcxStyle *cxStyle14;
        TcxStyle *cxStyle17;
        TcxStyle *cxStyle21;
        TcxStyle *cxStyle24;
        TcxStyle *cxStyle25;
        TcxStyle *cxStyle28;
        TcxStyle *cxStyle32;
        TcxStyle *cxStyle35;
        TcxStyle *cxStyle36;
        TcxStyle *cxStyle39;
        TcxStyle *cxStyle43;
        TcxStyle *cxStyle46;
        TcxStyle *cxStyle47;
        TcxStyle *cxStyle50;
        TcxStyle *cxStyle54;
        TcxStyle *cxStyle57;
        TcxStyle *cxStyle58;
        TcxStyle *cxStyle61;
        TcxStyle *cxStyle65;
        TcxStyle *cxStyle68;
        TcxStyle *cxStyle69;
        TcxStyle *cxStyle72;
        TcxStyle *cxStyle76;
        TcxStyle *cxStyle79;
        TcxStyle *cxStyle80;
        TcxStyle *cxStyle83;
        TcxStyle *cxStyle87;
        TcxStyle *cxStyle90;
        TcxStyle *cxStyle91;
        TcxStyle *cxStyle94;
        TcxStyle *cxStyle98;
        TcxStyle *cxStyle101;
        TcxStyle *cxStyle102;
        TcxStyle *cxStyle105;
        TcxStyle *cxStyle109;
        TcxStyle *cxStyle112;
        TcxStyle *cxStyle113;
        TcxStyle *cxStyle116;
        TcxStyle *cxStyle120;
        TcxStyle *cxStyle123;
        TcxStyle *cxStyle124;
        TcxStyle *cxStyle127;
        TcxStyle *cxStyle131;
        TcxStyle *cxStyle134;
        TcxStyle *cxStyle135;
        TcxStyle *cxStyle138;
        TcxStyle *cxStyle142;
        TcxStyle *cxStyle145;
        TcxStyle *cxStyle146;
        TcxStyle *cxStyle149;
        TcxStyle *cxStyle153;
        TcxStyle *cxStyle156;
        TcxStyle *cxStyle157;
        TcxStyle *cxStyle160;
        TcxStyle *cxStyle164;
        TcxStyle *cxStyle167;
        TcxStyle *cxStyle168;
        TcxStyle *cxStyle171;
        TcxStyle *cxStyle175;
        TcxStyle *cxStyle178;
        TcxStyle *cxStyle179;
        TcxStyle *cxStyle182;
        TcxStyle *cxStyle186;
        TcxStyle *cxStyle189;
        TcxStyle *cxStyle190;
        TcxStyle *cxStyle193;
        TcxStyle *cxStyle197;
        TcxStyle *cxStyle200;
        TcxStyle *cxStyle201;
        TcxStyle *cxStyle204;
        TcxStyle *cxStyle208;
        TcxStyle *cxStyle211;
        TcxStyle *cxStyle212;
        TcxStyle *cxStyle215;
        TcxStyle *cxStyle219;
        TcxStyle *cxStyle222;
        TcxStyle *cxStyle223;
        TcxStyle *cxStyle226;
        TcxStyle *cxStyle230;
        TcxStyle *cxStyle233;
        TcxStyle *cxStyle234;
        TcxStyle *cxStyle237;
        TcxStyle *cxStyle241;
        TcxStyle *cxStyle244;
        TcxStyle *cxStyle245;
        TcxStyle *cxStyle248;
        TcxStyle *cxStyle252;
        TcxStyle *cxStyle255;
        TcxStyle *cxStyle256;
        TcxStyle *cxStyle259;
        TcxStyle *cxStyle263;
        TcxStyle *cxStyle266;
        TcxStyle *cxStyle267;
        TcxStyle *cxStyle270;
        TcxStyle *cxStyle274;
        TcxStyle *cxStyle277;
        TcxStyle *cxStyle278;
        TcxStyle *cxStyle281;
        TcxStyle *cxStyle285;
        TcxStyle *cxStyle286;
        TcxStyle *cxStyle288;
        TcxStyle *cxStyle289;
        TcxStyle *cxStyle292;
        TcxStyle *cxStyle296;
        TcxStyle *cxStyle297;
        TcxStyle *cxStyle299;
        TcxStyle *cxStyle300;
        TcxStyle *cxStyle303;
        TcxStyle *cxStyle307;
        TcxStyle *cxStyle308;
        TcxStyle *cxStyle310;
        TcxStyle *cxStyle311;
        TcxStyle *cxStyle314;
        TcxStyle *cxStyle318;
        TcxStyle *cxStyle319;
        TcxStyle *cxStyle321;
        TcxStyle *cxStyle322;
        TcxStyle *cxStyle325;
        TcxStyle *cxStyle329;
        TcxStyle *cxStyle330;
        TcxStyle *cxStyle332;
        TcxStyle *cxStyle333;
        TcxStyle *cxStyle336;
        TcxStyle *cxStyle340;
        TcxStyle *cxStyle341;
        TcxStyle *cxStyle343;
        TcxStyle *cxStyle344;
        TcxStyle *cxStyle347;
        TcxStyle *cxStyle351;
        TcxStyle *cxStyle354;
        TcxStyle *cxStyle355;
        TcxStyle *cxStyle358;
        TcxStyle *cxStyle362;
        TcxPivotGridStyleSheet *PivotGridStyleSheetDevExpress;
        TcxPivotGridStyleSheet *PivotGridStyleSheetUserFormat1;
        TcxPivotGridStyleSheet *PivotGridStyleSheetUserFormat2;
        TcxPivotGridStyleSheet *PivotGridStyleSheetUserFormat3;
        TcxPivotGridStyleSheet *PivotGridStyleSheetUserFormat4;
        TcxPivotGridStyleSheet *PivotGridStyleSheetBrick;
        TcxPivotGridStyleSheet *PivotGridStyleSheetDesert;
        TcxPivotGridStyleSheet *PivotGridStyleSheetEggplant;
        TcxPivotGridStyleSheet *PivotGridStyleSheetLilac;
        TcxPivotGridStyleSheet *PivotGridStyleSheetMaple;
        TcxPivotGridStyleSheet *PivotGridStyleSheetMarinehighcolor;
        TcxPivotGridStyleSheet *PivotGridStyleSheetPlumhighcolor;
        TcxPivotGridStyleSheet *PivotGridStyleSheetPumpkinlarge;
        TcxPivotGridStyleSheet *PivotGridStyleSheetRainyDay;
        TcxPivotGridStyleSheet *PivotGridStyleSheetRedWhiteandBlueVGA;
        TcxPivotGridStyleSheet *PivotGridStyleSheetRose;
        TcxPivotGridStyleSheet *PivotGridStyleSheetRoselarge;
        TcxPivotGridStyleSheet *PivotGridStyleSheetSlate;
        TcxPivotGridStyleSheet *PivotGridStyleSheetSpruce;
        TcxPivotGridStyleSheet *PivotGridStyleSheetStormVGA;
        TcxPivotGridStyleSheet *PivotGridStyleSheetTealVGA;
        TcxPivotGridStyleSheet *PivotGridStyleSheetWheat;
        TcxPivotGridStyleSheet *PivotGridStyleSheetWindowsClassic;
        TcxPivotGridStyleSheet *PivotGridStyleSheetWindowsClassiclarge;
        TcxPivotGridStyleSheet *PivotGridStyleSheetWindowsStandard;
        TcxPivotGridStyleSheet *PivotGridStyleSheetWindowsStandardlarge;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrast1;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrast1large;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrast2;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrast2large;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrastBlack;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrastBlacklarge;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrastWhite;
        TcxPivotGridStyleSheet *PivotGridStyleSheetHighContrastWhitelarge;
        TcxStyleRepository *srUserDefined;
        TcxStyle *cxStyle378;
        TcxStyle *cxStyle379;
        TcxStyle *cxStyle380;
        TcxStyle *cxStyle381;
        TcxStyle *cxStyle382;
        TcxStyle *cxStyle383;
        TcxStyle *cxStyle384;
        TcxStyle *cxStyle385;
        TcxStyle *cxStyle386;
        TcxStyle *cxStyle387;
        TcxStyle *cxStyle388;
        TcxStyle *cxStyle389;
        TcxStyle *cxStyle390;
        TcxStyle *cxStyle391;
        TcxStyle *cxStyle392;
        TcxStyle *cxStyle393;
        TcxStyle *cxStyle394;
        TcxStyle *cxStyle395;
        TcxStyle *cxStyle396;
        TcxStyle *cxStyle397;
        TcxStyle *cxStyle398;
        TcxStyle *cxStyle399;
        TcxStyle *cxStyle400;
        TcxStyle *cxStyle401;
        TcxStyle *cxStyle402;
        TcxStyle *cxStyle403;
        TcxStyle *cxStyle404;
        TcxStyle *cxStyle405;
        TcxPivotGridStyleSheet *cxPivotGridStyleSheet1;
        TcxPivotGridStyleSheet *cxPivotGridStyleSheet2;
        TSplitter *Splitter;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall RadioGroupClick(TObject *Sender);
        void __fastcall lbPredefinedStyleSheetsClick(TObject *Sender);
        void __fastcall cbUserStyleSheetsChange(TObject *Sender);
        void __fastcall btnEditClick(TObject *Sender);
        void __fastcall btnLoadClick(TObject *Sender);
        void __fastcall btnSaveClick(TObject *Sender);
        void __fastcall FormActivate(TObject *Sender);
private:	// User declarations
    TcxCustomPivotGrid* __fastcall PivotGrid();

protected:
    void __fastcall ChangeVisibility(int AType);
    void __fastcall ClearUserDefinedStyleSheets();
    void __fastcall CreatePredefinedStyleSheetsList();
    void __fastcall CreateUserStyleSheetsList();
    TcxPivotGridStyleSheet* __fastcall GetCurrentStyleSheet();
    void __fastcall LoadUserDefinedStyleSheets(AnsiString AFileName);
    void __fastcall SaveUserDefinedStyleSheets(AnsiString AFileName);
    void __fastcall SetPredefinedStyleSheets();
    void __fastcall SetUserDefinedStyleSheets();
    void __fastcall UpdateGridStyleSheets(TcxPivotGridStyleSheet* AStyleSheet);
public:		// User declarations
	__fastcall TfrmStylesMulti(TComponent* Owner);
    virtual TcxLookAndFeelKind __fastcall GetDefaultLookAndFeelKind();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmStylesMulti *frmStylesMulti;
//---------------------------------------------------------------------------
#endif
