export const save = (key) => (value) => () => {
  localStorage.setItem(key, value)
}

export const load_ = ({ pure, none }) => (key) => () => {
  const value = localStorage.getItem(key)
  if (value === null) return none
  return pure(value)
}
