/** @babel */
/** @jsx etch.dom */
/* eslint-disable react/no-unknown-property */

import {CompositeDisposable} from 'atom'
import etch from 'etch'
import EtchComponent from './../etch-component'
import Octicon from 'etch-octicon'
import { openFile } from './../utils'

const defaultContent = 'To find usage, select an identifier and run the `golang:find-usages` command via the command palette.'

export default class UsageView extends EtchComponent {
  constructor (props) {
    super(props)
    this.subscriptions = new CompositeDisposable()
    if (props.model) {
      props.model.view = this
    }
  }

  render () {
    let style = 'white-space: pre-wrap; width: 100%;'
    if (this.props.model.orientation === 'vertical') {
      style = style + ' word-wrap: break-word;'
    }
    if (this.props.style) {
      style = style + ' ' + this.props.style
    }
    const packs = []
    if (this.props.content && this.props.content.referrers) {
      packs.push(...this.props.content.referrers.entries())
    }
    if (packs.length) {
      return this.structuredContent(style, packs)
    } else {
      return this.rawContent(style)
    }
  }

  structuredContent (style, packs) {
    return (
      <div ref='content' style={style} tabIndex='-1'>
        {packs.map(([pkg, refs]) => {
          return (
            <details className='go-plus-accordion-item' open>
              <summary className='go-plus-accordion-header'><Octicon name='package' /> {pkg} <span className='badge badge-info'>{refs.length}</span></summary>
              <main className='go-plus-accordion-content'>
                <table className='go-plus-table'>
                  {refs.map((ref) => {
                    return (
                      <tr onclick={this.handleClick.bind(this, ref)} className='go-plus-table-row'>
                        <td className='go-plus-table-cell'>{ref.text.trim()} <span className='text-subtle'>at {ref.filename}:{ref.row}:{ref.column}</span></td>
                      </tr>
                    )
                  })}
                </table>
              </main>
            </details>
          )
        })}
      </div>
    )
  }

  rawContent (style) {
    const content = this.props.content || defaultContent
    return (
      <div className='padded-content'>
        <span ref='content' style={style} tabIndex='-1'>
          {content}
        </span>
      </div>
    )
  }

  handleClick (ref) {
    openFile(ref.filename, {row: ref.row - 1, column: ref.column - 1}).catch((err) => {
      console.log('could not access ' + ref.filename, err)
    })
  }

  dispose () {
    this.destroy()
  }

  destroy () {
    super.destroy()
    this.subscriptions.dispose()
  }
}
