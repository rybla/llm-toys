export const undefinedJson = undefined

export const undefined_ = undefined
export const defined = a => a

export const optional = b => f => o => {
  if (o === undefined) return b
  else return f(o)
}

export const optional_ = b => f => o => {
  if (o === undefined) return b()
  else return f(o)
}