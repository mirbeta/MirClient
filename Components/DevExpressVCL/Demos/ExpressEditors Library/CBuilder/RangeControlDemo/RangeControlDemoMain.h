//---------------------------------------------------------------------------

#ifndef RangeControlDemoMainH
#define RangeControlDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxCheckBox.hpp"
#include "cxCheckComboBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
#include "cxTrackBar.hpp"
#include "dxRangeControl.hpp"
#include <Menus.hpp>
#include "BaseForm.h"
#include <Menus.hpp>
#include "cxCheckBox.hpp"
#include "cxCheckComboBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
#include "cxTrackBar.hpp"
#include "dxRangeControl.hpp"
#include <list>
//---------------------------------------------------------------------------
struct TChartData {
  Variant X;
  int Y;
};

typedef DynamicArray<TChartData> TChartDatas;
//---------------------------------------------------------------------------
class TdxRangeControlDemoForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGroupBox *cxGroupBox5;
	TcxGroupBox *cxGroupBox1;
	TdxRangeControl *rcNumericClient;
	TcxGroupBox *cxGroupBox6;
	TcxRadioGroup *rgNumberClientContentType;
	TcxTrackBar *tbNumericClient;
	TcxLabel *cxLabel1;
	TcxGroupBox *cxGroupBox2;
	TdxRangeControl *rcDateTimeClient;
	TcxGroupBox *cxGroupBox7;
	TcxRadioGroup *rgDateTimeClientContentType;
	TcxTrackBar *tbDateTimeClient;
	TcxLabel *cxLabel2;
	TcxGroupBox *cxGroupBox3;
	TdxRangeControl *rcDateTimeHeaderClient;
	TcxGroupBox *cxGroupBox4;
	TcxCheckBox *chbAutoFormatScaleCaptions;
	TcxCheckComboBox *cxCheckComboBox1;
	TcxLabel *cxLabel3;
	TMenuItem *Options1;
	TMenuItem *Animation1;
	TMenuItem *ShowRuler1;
	TMenuItem *ShowZoomscrollbar1;
	void __fastcall rgNumberClientContentTypePropertiesEditValueChanged(TObject *Sender);
	void __fastcall tbNumericClientPropertiesChange(TObject *Sender);
	void __fastcall rgDateTimeClientContentTypePropertiesEditValueChanged(TObject *Sender);
	void __fastcall tbDateTimeClientPropertiesChange(TObject *Sender);
	void __fastcall chbAutoFormatScaleCaptionsPropertiesEditValueChanged(TObject *Sender);
	void __fastcall cxCheckComboBox1PropertiesEditValueChanged(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Animation1Click(TObject *Sender);
	void __fastcall ShowRuler1Click(TObject *Sender);
	void __fastcall ShowZoomscrollbar1Click(TObject *Sender);
	void __fastcall rcNumericClientDrawContent(TdxCustomRangeControl *ASender, TcxCanvas *ACanvas,
          TdxRangeControlCustomClientViewInfo *AViewInfo, bool &AHandled);
	void __fastcall rcDateTimeClientDrawContent(TdxCustomRangeControl *ASender, TcxCanvas *ACanvas,
          TdxRangeControlCustomClientViewInfo *AViewInfo, bool &AHandled);
	void __fastcall rcDateTimeHeaderClientDrawContent(TdxCustomRangeControl *ASender,
          TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo,
          bool &AHandled);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);

private:	// User declarations
  TChartDatas FNumericClientData;
  TChartDatas FDateTimeClientData2;
  TChartDatas FDateTimeClientData1;
  TChartDatas FDateTimeHeaderClientData2;
  TChartDatas FDateTimeHeaderClientData1;
public:		// User declarations
	__fastcall TdxRangeControlDemoForm(TComponent* Owner);
	void __fastcall TdxRangeControlDemoForm::DrawNumericData(TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo,
	  TPoints APolygon, TRect R, TdxAlphaColor AColor);
	void __fastcall InitializeChartData();
	bool __fastcall IsNumberClientContentLineMode();
	bool __fastcall IsDateTimeClientContentLineMode();
	void __fastcall AddPolygonPoint(TPoints &APolygon, int X, int Y);
    void __fastcall GetPoints(TdxRangeControl *ARangeControl, TdxRangeControlCustomClientViewInfo *AViewInfo,
	  TRect R, TChartDatas ADataSource, TPoints &APolygon);
	void __fastcall TdxRangeControlDemoForm::DrawDateTimeData(
	  TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo, TPoints APolygon,
	  TRect R, TdxAlphaColor AColor, TdxAlphaColor ABrushColor);
	int __fastcall GetDateTimeHeaderValue(TChartDatas ADataSource, TdxRangeControlDateTimeHeaderClientContentElementViewInfo *AElement);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxRangeControlDemoForm *dxRangeControlDemoForm;
//---------------------------------------------------------------------------
#endif
