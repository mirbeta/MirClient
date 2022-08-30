//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoConvertH
#define EditorsStylesDemoConvertH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxPropertiesStore.hpp"
#include "cxTextEdit.hpp"
#include "EditorsStylesDemoBase.h"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxImage.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxProgressBar.hpp"
#include "cxSpinButton.hpp"
#include "cxSpinEdit.hpp"
#include "cxSplitter.hpp"
#include "cxTrackBar.hpp"
#include "dxZoomTrackBar.hpp"
#include "dxToggleSwitch.hpp"
#include <Dialogs.hpp>
#include <Graphics.hpp>
#include "EditorsStylesDemoUtils.h"
#include <ImgList.hpp>
#include <ActnList.hpp>
//---------------------------------------------------------------------------
class TEditorsStylesDemoConvertFrame : public TEditorsStylesDemoBaseFrame
{
__published:	// IDE-managed Components
  TPanel *Panel2;
  TcxGroupBox *gbConvertingOptions;
  TcxGroupBox *gbImageQuality;
  TcxLabel *lbBestQuality;
  TcxLabel *cxBestComp;
  TcxTrackBar *tbImageQuality;
  TcxSpinButton *sbImageQuality;
  TcxMaskEdit *meCompressionQuality;
  TcxCheckBox *cbGrayScale;
  TcxCheckBox *lbProgressive;
  TPanel *Panel3;
  TcxButton *bntConvert;
  TcxButton *btnPreview;
  TcxLabel *cxLabel2;
  TScrollBox *ScrollBox1;
  TcxImage *imgPreview;
  TcxGroupBox *gbDisplayOptions;
  TcxLabel *lbScale;
  TcxComboBox *cbScale;
  TcxSpinButton *sbScale;
  TcxSplitter *cxSplitter1;
  TPanel *pnlImage;
  TScrollBox *ScrollBox;
  TcxImage *cxImage;
  TPanel *Panel4;
  TcxButton *btnLoadImage;
  TcxGroupBox *cxGroupBox1;
  TcxProgressBar *cxProgressBar;
  TcxLabel *cxLabel1;
  TcxLabel *cxLabel3;
  TdxZoomTrackBar *dxZoomTrackBar;
  TcxTextEdit *cxTextEdit;
  TcxSplitter *cxSplitter2;
  TOpenDialog *OpenDialog;
  TSaveDialog *SaveDialog;
  TdxToggleSwitch *tsProgressive;
  TdxToggleSwitch *tsGrayScale;
  TActionList *ActionList1;
  TAction *aProgressive;
  TAction *aGrayScale;
  void __fastcall btnLoadImageClick(TObject *Sender);
  void __fastcall tbImageQualityPropertiesChange(TObject *Sender);
  void __fastcall dxZoomTrackBarPropertiesChange(TObject *Sender);
  void __fastcall cxTextEditPropertiesChange(TObject *Sender);
  void __fastcall sbImageQualityPropertiesChange(TObject *Sender);
  void __fastcall cbScalePropertiesChange(TObject *Sender);
  void __fastcall btnPreviewClick(TObject *Sender);
  void __fastcall bntConvertClick(TObject *Sender);
  void __fastcall cxTextEditExit(TObject *Sender);
  void __fastcall btnGetDrawParams(TcxCustomButton *Sender,
          TcxButtonState AState, TColor &AColor, TFont *AFont);
  void __fastcall sbScalePropertiesEditValueChanged(TObject *Sender);
  void __fastcall dxZoomTrackBarPropertiesDrawThumb(TObject *Sender,
          TcxCanvas *ACanvas, const TRect &ARect);
  void __fastcall dxZoomTrackBarPropertiesGetThumbRect(TObject *Sender,
          TRect &ARect);
  void __fastcall aProgressiveExecute(TObject *Sender);
  void __fastcall aGrayScaleExecute(TObject *Sender);
private:
  Graphics::TBitmap *FBitmap;
  TJPEGOptions __fastcall GetJPEGOptions();
  void __fastcall AdjustTrackBarThumb(TcxStyleSheetType AStyleSheetType,
    TdxZoomTrackBar* ATrackBar, Graphics::TBitmap* ABitmap);
public:
  __fastcall TEditorsStylesDemoConvertFrame(TComponent* Owner);
  __fastcall ~TEditorsStylesDemoConvertFrame();
  virtual void ChangeDisplayStyle(TcxStyleSheetType ADisplayStyle);
  TColor GetStyleBackgroundColor();
  String __fastcall Name();
  String __fastcall BriefName();
  String StylesIniPath();
  bool MenuOpenFileVisible();
  virtual void OpenFile(TObject *Sender);
  String Description();
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsStylesDemoConvertFrame *EditorsStylesDemoConvertFrame;
//---------------------------------------------------------------------------
#endif
