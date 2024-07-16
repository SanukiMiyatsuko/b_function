import { useState } from 'react';
import { ReactP5Wrapper, SketchProps } from 'react-p5-wrapper';
import './App.css';
import { abbrviate, dom, fund, less_than, T, term_to_string, Z } from "./code";
import { Scanner } from "./parse";
import ReactMarkdown from 'react-markdown';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import 'katex/dist/katex.min.css';
import { sketch_input, sketch_output } from './picture';

type Operation = "fund" | "dom" | "less_than";

export type MySketchProps = SketchProps & {
  inputstr: string;
  output: T;
  headSize: number;
  headDistance: number;
};

export type Options = {
  checkOnOffo: boolean;
  checkOnOffO: boolean;
  checkOnOffA: boolean;
  checkOnOffB: boolean;
  checkOnOffC: boolean;
  checkOnOffT: boolean;
  showHide: boolean;
};

export const headname: string = "B";
export const headnamereplace: string = "b";

function App() {
  const [inputA, setInputA] = useState("");
  const [inputB, setInputB] = useState("");
  const [Output, setOutput] = useState("出力：");
  const [outputObject, setOutputObject] = useState<T>(Z);
  const [outputError, setOutputError] = useState("");
  const [options, setOptions] = useState<Options>({
    checkOnOffo: false,
    checkOnOffO: false,
    checkOnOffA: false,
    checkOnOffB: false,
    checkOnOffC: false,
    checkOnOffT: false,
    showHide: false,
  });
  const [inputHeadSize, setinputHeadSize] = useState(50);
  const [inputHeadDistance, setinputHeadDistance] = useState(50);

  const compute = (operation: Operation) => {
    setOutput("");
    setOutputError("");
    try {
      const x = new Scanner(inputA).parse_term();
      const y = inputB ? new Scanner(inputB).parse_term() : null;

      let result;
      switch (operation) {
        case "fund":
          if (y === null) throw new Error("Bの入力が必要です");
          result = fund(x, y);
          break;
        case "dom":
          result = dom(x);
          break;
        case "less_than":
          if (y === null) throw new Error("Bの入力が必要です");
          setOutput(`出力：${less_than(x, y) ? "真" : "偽"}`);
          return;
        default:
          throw new Error("不明な操作");
      }

      setOutputObject(result);

      const outputString = abbrviate(term_to_string(result, options), options);
      setOutput(`出力：${options.checkOnOffT ? `$${outputString}$` : outputString}`);
    } catch (error) {
      if (error instanceof Error) setOutputError(error.message);
      else setOutputError("不明なエラー");
    }
  };

  const handleCheckboxChange = (key: keyof Options) => {
    setOptions((prevOptions) => ({
      ...prevOptions,
      [key]: !prevOptions[key],
    }));
  };

  return (
    <div className="app">
      <header>B関数計算機</header>
      <main>
        <p className="rdm">
          入力は{headname}(a,b), {headname}_&#123;a&#125;(b)の形式で行ってください。<br />
          a=0の時は{headname}(b)としても大丈夫です。<br />
          _, &#123;, &#125;は省略可能です。<br />
          略記として、1 := {headname}(0,0), n := 1 + 1 + ...(n個の1)... + 1, ω := {headname}(0,1), Ω := {headname}(1,0)が使用可能。<br />
          また、{headname}は"{headnamereplace}"で、ωはwで、ΩはWで代用可能。
        </p>
        A:
        <input
          className="input is-primary"
          value={inputA}
          onChange={(e) => setInputA(e.target.value)}
          type="text"
          placeholder="入力A"
        />
        B:
        <input
          className="input is-primary"
          value={inputB}
          onChange={(e) => setInputB(e.target.value)}
          type="text"
          placeholder="入力B"
        />
        <div className="block">
          <button className="button is-primary" onClick={() => compute("fund")}>
            A[B]を計算
          </button>
          <button className="button is-primary" onClick={() => compute("dom")}>
            dom(A)を計算
          </button>
          <button className="button is-primary" onClick={() => compute("less_than")}>
            A &lt; Bか判定
          </button>
        </div>
        <input type="button" value="オプション" onClick={() => handleCheckboxChange('showHide')} className="button is-primary is-light is-small" />
        {options.showHide ? (
          <ul>
            <li><label className="checkbox">
              <input type="checkbox" checked={options.checkOnOffo} onChange={() => handleCheckboxChange('checkOnOffo')} />
              &nbsp;{headname}(0,1)をωで出力
            </label></li>
            <li><label className="checkbox">
              <input type="checkbox" checked={options.checkOnOffO} onChange={() => handleCheckboxChange('checkOnOffO')} />
              &nbsp;{headname}(1,0)をΩで出力
            </label></li>
            <li><label className="checkbox">
              <input type="checkbox" checked={options.checkOnOffA} onChange={() => handleCheckboxChange('checkOnOffA')} />
              &nbsp;{headname}(a,b)を{headname}_a(b)で表示
            </label></li>
            {options.checkOnOffA ? (
              <li><ul><li><label className="checkbox">
                <input type="checkbox" checked={options.checkOnOffB} onChange={() => handleCheckboxChange('checkOnOffB')} />
                &nbsp;全ての&#123; &#125;を表示
              </label></li></ul></li>
            ) : (
              <></>
            )}
            <li><label className="checkbox">
              <input type="checkbox" checked={options.checkOnOffC} onChange={() => handleCheckboxChange('checkOnOffC')} />
              &nbsp;{headname}(0,b)を{headname}(b)で表示
            </label></li>
            <li><label className="checkbox">
              <input type="checkbox" checked={options.checkOnOffT} onChange={() => handleCheckboxChange('checkOnOffT')} />
              &nbsp;TeXで出力
            </label></li>
          </ul>
        ) : (
          <></>
        )}
        <div className="box is-primary">
          {outputError !== "" ? (
            <div className="notification is-danger">{outputError}</div>
          ) : (
            <div>
              <ReactMarkdown
                remarkPlugins={[remarkMath]}
                rehypePlugins={[rehypeKatex]}
              >
                {Output}
              </ReactMarkdown>
            </div>
          )}
        </div>
        <div className='hydra'>
          ノードの大きさ：
          <input
            className="hydraSize"
            value={inputHeadSize}
            onChange={(e) => setinputHeadSize(parseInt(e.target.value))}
            min="0"
            max="200"
            type="range"
          /><br />
          ノード間の距離：
          <input
            className="hydraSize"
            value={inputHeadDistance}
            onChange={(e) => setinputHeadDistance(parseInt(e.target.value))}
            min="0"
            max="200"
            type="range"
          /><br />
        </div>
        <div className='sketchCanvas'>
          <ReactP5Wrapper sketch={sketch_input} inputstr={inputA} headSize={inputHeadSize} headDistance={inputHeadDistance} />
          <ReactP5Wrapper sketch={sketch_output} output={outputObject} headSize={inputHeadSize} headDistance={inputHeadDistance} />
        </div>
      </main>
      <footer>
        <a href="https://googology.fandom.com/ja/wiki/%E3%83%A6%E3%83%BC%E3%82%B6%E3%83%BC%E3%83%96%E3%83%AD%E3%82%B0:%E7%AB%B9%E5%8F%96%E7%BF%81/Goal%E9%96%A2%E6%95%B0" target="_blank" rel="noreferrer">Definition of "Goal Function"</a> by <a href="https://googology.fandom.com/ja/wiki/%E3%83%A6%E3%83%BC%E3%82%B6%E3%83%BC:%E7%AB%B9%E5%8F%96%E7%BF%81" target="_blank" rel="noreferrer">竹取翁</a>, Retrieved 2024/06/6 <br />
        The program <a href="https://github.com/SanukiMiyatsuko/goal_function" target="_blank" rel="noreferrer">https://github.com/SanukiMiyatsuko/goal_function</a> is licensed by <a href="https://creativecommons.org/licenses/by-sa/3.0/legalcode" target="_blank" rel="noreferrer">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.<br />
        Last updated: 2024/07/17
      </footer>
    </div>
  );
}

export default App;